module FxTrade ( initFxTradeData
               , backTest
               , learning
               , trade
               , gaLearningEvaluate
               ) where
  
import Debug.Trace
import qualified Data.Map                 as M
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxTechnicalAnalysisData  as Fad
import qualified FxTradeData              as Ftd
import qualified FxTechnicalAnalysis      as Ta
import qualified GlobalSettingData        as Gsd
import qualified GlobalSettingFunction    as Gsf
import qualified Tree                     as Tr
import qualified FxSetting                as Fs
import qualified FxMongodb                as Fm
import qualified FxPrint                  as Fp

evaluateProfitInc :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitInc fts ftad =
  Tr.evaluateTree fst (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

evaluateProfitDec :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitDec fts ftad =
  Tr.evaluateTree snd (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

resetCounter :: Ftd.FxTradeData -> Ftd.FxTradeData
resetCounter td =
  td { Ftd.trTradeDate    = 0
     , Ftd.trTrade        = 0
     , Ftd.alcOpen        = Fad.zeroFxalgorithmListCount
     , Ftd.alcCloseProfit = Fad.zeroFxalgorithmListCount
     , Ftd.alcCloseLoss   = Fad.zeroFxalgorithmListCount
     }
  
initFxTradeData :: Ftd.FxEnvironment -> Ftd.FxTradeData
initFxTradeData Ftd.Backtest =
  Ftd.initFxTradeDataCommon { Ftd.environment      = Ftd.Backtest
                            , Ftd.bearer           = ""
                            , Ftd.url              = ""
                        }
initFxTradeData Ftd.Practice =
  Ftd.initFxTradeDataCommon { Ftd.environment      = Ftd.Practice
                            , Ftd.bearer           = Gsd.tradePracticeBearer Gsd.gsd
                            , Ftd.url              = Gsd.tradePracticeUrl  Gsd.gsd
                        }
initFxTradeData Ftd.Production =
  Ftd.initFxTradeDataCommon { Ftd.environment      = Ftd.Production
                            , Ftd.bearer           = Gsd.tradeProductionBearer Gsd.gsd
                            , Ftd.url              = Gsd.tradeProductionUrl  Gsd.gsd
                            }
    
evaluate :: Fad.FxChartTaData -> 
            Fsd.FxSettingData ->
            (Ftd.FxTradeData -> Double -> Double) ->
            Bool ->
            Ftd.FxTradeData ->
            (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
evaluate ctd fsd f1 forceSell td = do
  let cd     = Fad.taChart ctd
      chart  = Fcd.close   cd
      rate   = Fcd.close $ Ftd.rate td
      ftado  = Fad.open        ctd
      ftadcp = Fad.closeProfit ctd
      ftadcl = Fad.closeLoss   ctd
      fto    = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp   = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl   = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      ltt = Fs.getLearningTestTime fsd
      (position, open)
        = if Gsf.buyEvaluation td chart rate &&
             evaluateProfitInc fto ftado
          then (chart, Ftd.Buy)
          else if Gsf.sellEvaluation td chart rate &&
                  evaluateProfitDec fto ftado
               then (chart, Ftd.Sell)
               else (0, Ftd.None)
      (profits, realizedPL, close)
        = if open /= Ftd.None && rate /= 0
          then if Ftd.side td == Ftd.Buy
               then (chart - rate, (chart / rate) - 1, Ftd.Close)
               else if Ftd.side td == Ftd.Sell
                    then (rate - chart, 1 - (chart / rate), Ftd.Close)
                    else (0, 0, Ftd.None)
          else if rate /= 0
               then if Ftd.side td == Ftd.Buy &&
                       (forceSell ||
                       (Fs.getSimChartMax fsd < Fcd.no cd - (Fcd.no $ Ftd.rate td) && 
                        ((0 <= chart - rate && evaluateProfitDec ftcp ftadcp) ||
                         (chart - rate <= 0 && evaluateProfitDec ftcl ftadcl))) ||
                         ltt * Gsd.learningTestCount Gsd.gsd < Fcd.no cd - (Fcd.no $ Ftd.rate td))
                    then (chart - rate, (chart / rate) - 1, Ftd.Buy)
                    else if Ftd.side td == Ftd.Sell &&
                            (forceSell || 
                             (Fs.getSimChartMax fsd < Fcd.no cd - (Fcd.no $ Ftd.rate td) && 
                              ((0 <= rate - chart && evaluateProfitInc ftcp ftadcp) ||
                               (rate - chart <= 0 && evaluateProfitInc ftcl ftadcl))) ||
                              ltt * Gsd.learningTestCount Gsd.gsd < Fcd.no cd - (Fcd.no $ Ftd.rate td))
                         then (rate - chart, 1 - (chart / rate), Ftd.Sell)
                         else (0, 0, Ftd.None)
               else (0, 0, Ftd.None)
      td' = td { Ftd.chart  = cd
               , Ftd.rate   = if open == Ftd.Buy
                              then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                       , Fcd.close = position + Gsd.spread Gsd.gsd
                                                       }
                              else if open == Ftd.Sell
                                   then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                            , Fcd.close = position - Gsd.spread Gsd.gsd
                                                            }
                                   else if close /= Ftd.None
                                        then Fcd.initFxChartData
                                        else Ftd.rate td
               , Ftd.alcOpen =
                   Fad.FxalgorithmListCount { Fad.prev =
                                                if open == Ftd.Buy 
                                                then Ta.makeValidLeafDataMapInc fto ftado
                                                else if open == Ftd.Sell
                                                     then Ta.makeValidLeafDataMapDec fto ftado
                                                     else if close /= Ftd.None
                                                          then ([], M.empty)
                                                          else Fad.prev $ Ftd.alcOpen td
                                            , Fad.listCount =
                                                if close /= Ftd.None
                                                then Ta.addFxalgorithmListCount profits (Fad.prev $ Ftd.alcOpen td)
                                                     (Fad.listCount $ Ftd.alcOpen td)
                                                else Fad.listCount $ Ftd.alcOpen td
                                            }
               , Ftd.alcCloseProfit =
                   Fad.FxalgorithmListCount { Fad.prev = ([], M.empty)
                                            , Fad.listCount =
                                                if close == Ftd.Buy && 0 < profits
                                                then Ta.addFxalgorithmListCount profits (Ta.makeValidLeafDataMapDec ftcp ftadcp)
                                                     (Fad.listCount $ Ftd.alcCloseProfit td)
                                                else if close == Ftd.Sell && 0 < profits
                                                     then Ta.addFxalgorithmListCount profits (Ta.makeValidLeafDataMapInc ftcp ftadcp)
                                                          (Fad.listCount $ Ftd.alcCloseProfit td)
                                                     else Fad.listCount $ Ftd.alcCloseProfit td
                                            }
               , Ftd.alcCloseLoss =
                   Fad.FxalgorithmListCount { Fad.prev = ([], M.empty)
                                            , Fad.listCount =
                                                if close == Ftd.Buy && profits < 0
                                                then Ta.addFxalgorithmListCount (-1 / profits)
                                                     (Ta.makeValidLeafDataMapDec ftcl ftadcl)
                                                     (Fad.listCount $ Ftd.alcCloseLoss td)
                                                else if close == Ftd.Sell && profits < 0
                                                     then Ta.addFxalgorithmListCount (-1 / profits)
                                                          (Ta.makeValidLeafDataMapInc ftcl ftadcl)
                                                          (Fad.listCount $ Ftd.alcCloseLoss td)
                                                     else Fad.listCount $ Ftd.alcCloseLoss td
                                            }
               , Ftd.side  = if open == Ftd.Buy
                             then Ftd.Buy
                             else if open == Ftd.Sell
                                  then Ftd.Sell
                                  else if close /= Ftd.None
                                       then Ftd.None
                                       else Ftd.side td
               , Ftd.trTradeDate = if close /= Ftd.None
                                   then Ftd.trTradeDate td + Fcd.no cd - (Fcd.no $ Ftd.rate td)
                                   else Ftd.trTradeDate td
               , Ftd.trTrade     = if close /= Ftd.None
                                  then Ftd.trTrade td + 1
                                  else Ftd.trTrade td
               , Ftd.trSuccess  = if close /= Ftd.None && 0 < profits
                                  then Ftd.trSuccess td + 1
                                  else Ftd.trSuccess td
               , Ftd.trFail     = if close /= Ftd.None && profits <= 0
                                  then Ftd.trFail td + 1
                                  else Ftd.trFail td
               , Ftd.profit     = Ftd.profit td + profits
               , Ftd.realizedPL = Ftd.realizedPL td + 25 * (f1 td chart) * realizedPL
               , Ftd.unrealizedPL = if (Fcd.close $ Ftd.rate td') /= 0
                                    then if Ftd.side td' == Ftd.Buy
                                         then Ftd.realizedPL td' + 25 * (f1 td' chart) * ((chart / (Fcd.close $ Ftd.rate td')) - 1)
                                         else if Ftd.side td' == Ftd.Sell
                                              then Ftd.realizedPL td' + 25 * (f1 td' chart) * (1 - (chart / (Fcd.close $ Ftd.rate td')))
                                              else Ftd.realizedPL td'
                                    else Ftd.realizedPL td'
               }
    in (open, close, td')

{-
(x:xcd), ftado, ftadcp, ftadcl [new .. old]
return [old .. new]
-}

makeChartTa :: [Fcd.FxChartData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               [Fad.FxChartTaData] ->
               [Fad.FxChartTaData]
makeChartTa [] _ _ _ ctdl = ctdl
makeChartTa (x:xcd) ftado ftadcp ftadcl ctdl =
  let ftado'  = M.map (\a -> dropWhile (\b -> Fcd.no x <= (Fcd.no $ Fad.chart b)) a) ftado
      ftadcp' = M.map (\a -> dropWhile (\b -> Fcd.no x <= (Fcd.no $ Fad.chart b)) a) ftadcp
      ftadcl' = M.map (\a -> dropWhile (\b -> Fcd.no x <= (Fcd.no $ Fad.chart b)) a) ftadcl
      ctd = Fad.FxChartTaData { Fad.taChart     = x 
                              , Fad.open        = M.map (\y -> if null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else head y) ftado'
                              , Fad.closeProfit = M.map (\y -> if null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else head y) ftadcp'
                              , Fad.closeLoss   = M.map (\y -> if null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else head y) ftadcl'
                              }
  in makeChartTa xcd ftado' ftadcp' ftadcl' (ctd:ctdl)

{-
xs [old .. new]
return [old .. new]

Prelude> break (\x -> x `mod` 5 == 0 ) [1..10]
([1,2,3,4],[5,6,7,8,9,10])
-}

makeSimChart :: Int -> [Fcd.FxChartData] -> [Fcd.FxChartData]
makeSimChart _ [] = []
makeSimChart c xs =
  let (chart, xs') = break (\x -> Fcd.no x `mod` c == 0) xs
  in if null xs'
     then let high = maximum $ map (\x -> (Fcd.high x, Fcd.no x)) chart
              low  = minimum $ map (\x -> (Fcd.low x, Fcd.no x)) chart
              fcdHigh  = (head chart) { Fcd.no = snd high
                                      , Fcd.close = fst high
                                      }
              fcdLow   = (head chart) { Fcd.no = snd low
                                      , Fcd.close = fst low
                                      }
              fcdClose = last chart
          in [fcdClose]
{-              
          in if snd high < snd low
             then [fcdHigh, fcdLow, fcdClose]
             else [fcdLow,  fcdHigh, fcdClose]
-}
     else if null chart
          then (head xs') : (makeSimChart c $ tail xs')
          else let chart' = head xs' : chart
                   high = maximum $ map (\x -> (Fcd.high x, Fcd.no x)) chart'
                   low  = minimum $ map (\x -> (Fcd.low x, Fcd.no x)) chart'
                   fcdHigh  = (head xs') { Fcd.no = snd high
                                         , Fcd.close = fst high
                                         }
                   fcdLow   = (head xs') { Fcd.no = snd low
                                         , Fcd.close = fst low
                                         }
                   fcdClose = head xs'
               in fcdClose : (makeSimChart c $ tail xs')
{-                   
               in if snd high < snd low
                  then fcdHigh : fcdLow : fcdClose : (makeSimChart c $ tail xs')
                  else fcdLow : fcdHigh : fcdClose : (makeSimChart c $ tail xs')
-}

{-
xcd [old .. new]
ftado, ftadcp, ftadcl [new .. old]
return [old .. new]
-}

makeChart :: Fsd.FxSettingData -> Int -> [Fcd.FxChartData] -> [Fad.FxChartTaData]
makeChart fsd chartLength xcd  =
  let fs   = Fsd.fxSetting fsd
      ftado  = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaOpen fs
      ftadcp = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseProfit fs
      ftadcl = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseLoss fs
  in makeChartTa (take chartLength $ reverse xcd) ftado ftadcp ftadcl []

backTest :: Bool ->
            Int ->
            Int ->
            Int ->
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            [Fcd.FxChartData] ->
            IO (Bool, Ftd.FxTradeData, Fsd.FxSettingData)
backTest latest endN l s td fsd xcd = do
  let ctdl = makeChart fsd l xcd
  td'' <- foldl (\a ctd -> do td' <- a
                              let (open, close, td3) = if Ftd.side td' == Ftd.None &&
                                                          (Ftd.trSuccess td + s < Ftd.trSuccess td' ||
                                                           Ftd.trFail td + s < Ftd.trFail td')
                                                       then (Ftd.None, Ftd.None, td')
                                                       else evaluate ctd fsd Gsf.getQuantityBacktest False td'
                              if latest && (open /= Ftd.None || close /= Ftd.None)
                                then Fp.printTradeResult open close td' td3 0
                                else return ()
                              return td3)
                     (pure td) ctdl
  clear <- Fm.checkFxSettingData
  fsd' <- Fm.updateFxSettingData $ Fs.updateFxSettingData ctdl td td'' fsd
  return (clear, resetCounter td'', fsd')
-- traceShow(Fcd.close $ Ftd.chart td', Fcd.close $ Ftd.rate td', Ftd.profit td', Ftd.side td') $ 

learning :: Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            Ftd.FxTradeData
learning td fsd = 
  let fc = Fsd.fxChart fsd
      ctdl = makeChart fsd (Fsd.chartLength fc) (Fsd.chart fc)
      (_, _, td'') = foldl (\(_, _, td') ctd -> evaluate ctd fsd Gsf.getQuantityLearning False td')
                     (Ftd.None, Ftd.None, td) $ init ctdl
      (_, _, td''') = evaluate (last ctdl) fsd Gsf.getQuantityLearning False td''
  in if null ctdl
     then td
     else td'''
          
trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         [Fcd.FxChartData] ->
         (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
trade td fsd xcd = 
  let ctdl = makeChart fsd 1 xcd
      (open, close, td') = evaluate (last ctdl) fsd Gsf.getQuantityLearning False td
  in (open, close, resetCounter td')
  
gaLearningEvaluate :: Fsd.FxSettingData -> (Fsd.FxSettingData, Rational)
gaLearningEvaluate fsd =
  let td = learning (initFxTradeData Ftd.Backtest) fsd
  in (fsd, toRational $ Gsf.getEvaluationValue td)

  
