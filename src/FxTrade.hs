module FxTrade ( resetFxalgorithmListCount
               , initFxTradeData
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

evaluateProfitInc :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitInc fts ftad =
  Tr.evaluateTree fst (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

evaluateProfitDec :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitDec fts ftad =
  Tr.evaluateTree snd (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

resetFxalgorithmListCount :: Ftd.FxTradeData -> Ftd.FxTradeData
resetFxalgorithmListCount td =
  td { Ftd.alcOpen        = Fad.zeroFxalgorithmListCount
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
            Bool ->
            Ftd.FxTradeData ->
            (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
evaluate ctd fsd f1 forceSell onlySell td = do
  let cd     = Fad.taChart ctd
      chart  = Fcd.close   cd
      rate   = Fcd.close $ Ftd.rate td
      ftado  = Fad.open        ctd
      ftadcp = Fad.closeProfit ctd
      ftadcl = Fad.closeLoss   ctd
      fto    = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp   = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl   = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      (profitb, position, open)
        = if Gsf.buyEvaluation td chart rate &&
             evaluateProfitInc fto ftado && not onlySell
          then (-Gsd.spread Gsd.gsd, chart, Ftd.Buy)
          else if Gsf.sellEvaluation td chart rate &&
                  evaluateProfitDec fto ftado && not onlySell
               then (-Gsd.spread Gsd.gsd, chart, Ftd.Sell)
               else (0, 0, Ftd.None)
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
                        (0 < chart - rate  && evaluateProfitDec ftcp ftadcp) ||
                        (chart - rate < 0  && evaluateProfitDec ftcl ftadcl) ||
                         Gsd.maxTradePeriod Gsd.gsd < Fcd.date cd - (Fcd.date $ Ftd.rate td))
                    then (chart - rate, (chart / rate) - 1, Ftd.Buy)
                    else if Ftd.side td == Ftd.Sell &&
                            (forceSell || 
                             (0 < rate - chart  && evaluateProfitInc ftcp ftadcp) ||
                              (rate - chart < 0 && evaluateProfitInc ftcl ftadcl) ||
                              Gsd.maxTradePeriod Gsd.gsd < Fcd.date cd - (Fcd.date $ Ftd.rate td))
                         then (rate - chart, 1 - (chart / rate), Ftd.Sell)
                         else (0, 0, Ftd.None)
               else (0, 0, Ftd.None)
      td' = td { Ftd.chart  = cd
               , Ftd.rate   = if open == Ftd.Buy
                              then Fcd.initFxChartData { Fcd.date  = Fcd.date cd
                                                       , Fcd.close = position + Gsd.spread Gsd.gsd
                                                       }
                              else if open == Ftd.Sell
                                   then Fcd.initFxChartData { Fcd.date  = Fcd.date cd
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
               , Ftd.trSuccessDate = if close /= Ftd.None && 0 < profits
                                     then Ftd.trSuccessDate td + Fcd.date cd - (Fcd.date $ Ftd.rate td)
                                     else Ftd.trSuccessDate td
               , Ftd.trSuccess  = if close /= Ftd.None && 0 < profits
                                  then Ftd.trSuccess td + 1
                                  else Ftd.trSuccess td
               , Ftd.trFail     = if close /= Ftd.None && profits <= 0
                                  then Ftd.trFail td + 1
                                  else Ftd.trFail td
               , Ftd.profit     = Ftd.profit td + profits + profitb
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

makeChartTa :: [Fcd.FxChartData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               M.Map Int [Fad.FxTechnicalAnalysisData] ->
               [Fad.FxChartTaData] ->
               [Fad.FxChartTaData]
makeChartTa [] _ _ _ ctdl = ctdl
makeChartTa (x:xcd) ftado ftadcp ftadcl ctdl =
  let ftado'  = M.map (\a -> dropWhile (\b -> Fcd.date x < (Fcd.date $ Fad.chart b)) a) ftado
      ftadcp' = M.map (\a -> dropWhile (\b -> Fcd.date x < (Fcd.date $ Fad.chart b)) a) ftadcp
      ftadcl' = M.map (\a -> dropWhile (\b -> Fcd.date x < (Fcd.date $ Fad.chart b)) a) ftadcl
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

makeChart :: Fsd.FxSettingData -> Int -> [Fcd.FxChartData] -> [Fad.FxChartTaData]
makeChart fsd chartLength xcd  =
  let fs   = Fsd.fxSetting fsd
      ftado  = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fcd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaOpen fs
      ftadcp = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fcd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseProfit fs
      ftadcl = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fcd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseLoss fs
  in makeChartTa (take chartLength $ reverse xcd) ftado ftadcp ftadcl []

backTest :: Int ->
            Int ->
            Int ->
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            [Fcd.FxChartData] ->
            IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTest l ls s td fsd xcd = do
  let ctdl = makeChart fsd (l + ls) xcd
      (_, _, td'') = foldl (\(_, _, td') ctd -> if Ftd.trSuccess td + s < Ftd.trSuccess td' ||
                                                   l < (Fcd.date $ Ftd.chart td') - (Fcd.date $ Ftd.chart td)
                                                then if Ftd.side td' == Ftd.None 
                                                     then (Ftd.None, Ftd.None, td')
                                                     else evaluate ctd fsd Gsf.getQuantityBacktest False True td'
                                                else evaluate ctd fsd Gsf.getQuantityBacktest False False td') (Ftd.None, Ftd.None, td) ctdl
  fsd' <- Fm.updateFxSettingData $ Fs.updateFxSettingData ctdl td td'' fsd
  return (td'', fsd')
-- traceShow(Fcd.close $ Ftd.chart td', Fcd.close $ Ftd.rate td', Ftd.profit td', Ftd.side td') $ 

learning :: Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            Ftd.FxTradeData
learning td fsd = 
  let fc = Fsd.fxChart fsd
      ctdl = makeChart fsd (Fsd.chartLength fc) (Fsd.chart fc)
      (_, _, td'') = foldl (\(_, _, td') ctd -> evaluate ctd fsd Gsf.getQuantityLearning False False td')
                     (Ftd.None, Ftd.None, td) $ init ctdl
      (_, _, td''') = evaluate (last ctdl) fsd Gsf.getQuantityLearning True False td''
  in if null ctdl
     then td
     else td'''
          
trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         [Fcd.FxChartData] ->
         (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
trade td fsd xcd = 
  let ctdl = makeChart fsd 1 xcd
  in evaluate (head ctdl) fsd Gsf.getQuantityBacktest False False td
  
gaLearningEvaluate :: Fsd.FxSettingData -> (Fsd.FxSettingData, Rational)
gaLearningEvaluate fsd =
  let td = learning (initFxTradeData Ftd.Backtest) fsd
  in (fsd, toRational $ Gsf.getEvaluationValue td)

  
