module FxTrade ( initFxTradeData
               , backTest
               , learning
               , trade
               , gaLearningEvaluate
               , evaluationOk
               ) where

import           Control.Monad
import qualified Data.Map                as M
import qualified Data.List               as L
import           Data.Vector             as V
import           Debug.Trace
import           Control.Monad.Random    as R
import qualified FxChartData             as Fcd
import qualified FxMongodb               as Fm
import qualified FxPrint                 as Fp
import qualified FxSettingData           as Fsd
import qualified FxTechnicalAnalysis     as Ta
import qualified FxTechnicalAnalysisData as Fad
import qualified FxTradeData             as Ftd
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

evaluationOk :: [Ftd.FxTradeData] -> Bool
evaluationOk tdlt =
  (L.and $ L.map (\x -> Gsd.initalProperty Gsd.gsd  < Ftd.realizedPL x) tdlt)
  -- (and $ map (\x -> 0 < Ftd.getEvaluationValue x) tdlt) -- && 
  --0 < Ftd.getEvaluationValueList tdlt
  
getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest td chart = if (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25 < Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd
                               then (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = Ftd.realizedPL td

{-

getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest  _ _ = Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd



getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest td chart = Ftd.realizedPL td


getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning  _ _ = Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd


getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning _ _ = Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
-}

evaluateProfitInc :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitInc fts ftad =
  Tr.evaluateTree fst (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

evaluateProfitDec :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitDec fts ftad =
  Tr.evaluateTree snd (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

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
evaluate ctd fsd f1 forceSell td =
  let cd        = Fad.taChart ctd
      chart     = Fcd.close cd
      chartHigh = Fcd.close cd
      chartLow  = Fcd.close cd
      tradeRate = Fcd.close $ Ftd.tradeRate td
      tradeDate = Fcd.no cd - (Fcd.no $ Ftd.tradeRate td)
      ftado     = Fad.open        ctd
      ftadcp    = Fad.closeProfit ctd
      ftadcl    = Fad.closeLoss   ctd
      fto       = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp      = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl      = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      fs        = Ftd.fxSetting td
      lcd = Gsd.maxTradeTime Gsd.gsd
      unrealizedPL
        | Ftd.side td == Ftd.Buy  = Ftd.realizedPL td + 25 * f1 td chartLow * ((chartLow / tradeRate) - 1)
        | Ftd.side td == Ftd.Sell = Ftd.realizedPL td + 25 * f1 td chartHigh * (1 - (chartHigh / tradeRate))
        | otherwise = Ftd.realizedPL td
      (position, open)
        | Ftd.side td == Ftd.None && evaluateProfitInc fto ftado = (chart, Ftd.Buy)
        | Ftd.side td == Ftd.None && evaluateProfitDec fto ftado = (chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)
{-
        | (Ftd.side td == Ftd.None ||
           (Ftd.side td == Ftd.Sell && (0 < tradeRate - chartHigh ||
                                        tradeRate - chartHigh < 0))) &&
          evaluateProfitInc fto ftado = (chartHigh, Ftd.Buy)
        | (Ftd.side td == Ftd.None ||
           (Ftd.side td == Ftd.Buy && (0 < chartLow - tradeRate ||
                                       chartLow - tradeRate <= 0))) &&
          evaluateProfitDec fto ftado = (chartLow, Ftd.Sell)
        | otherwise = (0, Ftd.None)
-}        
      (profits, close)
        | open /= Ftd.None && Ftd.side td == Ftd.Buy  = (chartLow - tradeRate, Ftd.Buy)
        | open /= Ftd.None && Ftd.side td == Ftd.Sell = (tradeRate - chartHigh, Ftd.Sell)
        | Ftd.side td == Ftd.Buy &&
          (forceSell || lcd < tradeDate ||
           (0 < chartLow - tradeRate  && evaluateProfitDec ftcp ftadcp) ||
           (chartLow - tradeRate < 0 && evaluateProfitDec ftcl ftadcl)) = (chartLow - tradeRate, Ftd.Buy)
        | Ftd.side td == Ftd.Sell &&
          (forceSell || lcd < tradeDate ||
            (0 < tradeRate - chartHigh && evaluateProfitInc ftcp ftadcp) ||
            (tradeRate - chartHigh < 0 && evaluateProfitInc ftcl ftadcl)) = (tradeRate - chartHigh, Ftd.Sell)
        | otherwise = (0, Ftd.None)
      fs' = if close /= Ftd.None
            then let ls  = Fsd.learningSetting fs
                     ls' = ls { Fsd.totalTradeDate     = Fsd.totalTradeDate ls + tradeDate
                              , Fsd.numTraderadeDate   = Fsd.numTraderadeDate ls + 1
                              }
                     alcOpen
                       | 0 < profits = Ta.calcFxalgorithmListCount profits $ Fsd.prevOpen fs
                       | otherwise   = (Tr.emptyLeafDataMap, M.empty)
                     alcCloseProfit
                       | close == Ftd.Buy  && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapDec ftcp ftadcp
                       | close == Ftd.Sell && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapInc ftcp ftadcp
                       | otherwise         = (Tr.emptyLeafDataMap, M.empty)
                     alcCloseLoss
                       | close == Ftd.Buy  && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapDec ftcl ftadcl
                       | close == Ftd.Sell && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapInc ftcl ftadcl
                       | otherwise          = (Tr.emptyLeafDataMap, M.empty)
                     fxTaOpen        = Ta.updateAlgorithmListCount Fad.open
                                       ctd alcOpen        $ Fsd.fxTaOpen fs
                     fxTaCloseProfit = Ta.updateAlgorithmListCount Fad.closeProfit
                                       ctd alcCloseProfit $ Fsd.fxTaCloseProfit fs
                     fxTaCloseLoss   = Ta.updateAlgorithmListCount Fad.closeLoss
                                       ctd alcCloseLoss   $ Fsd.fxTaCloseLoss fs
                 in {- traceShow(tradeDate, Fcd.no cd, (Fcd.no $ Ftd.tradeRate td), Fsd.trTradeDate ls' `div` Fsd.trTrade ls', Fsd.trTradeDate ls', Fsd.trTrade ls') $ -}
                    fs { Fsd.learningSetting  = ls'
                       , Fsd.fxTaOpen         = fxTaOpen       
                       , Fsd.fxTaCloseProfit  = fxTaCloseProfit
                       , Fsd.fxTaCloseLoss    = fxTaCloseLoss  
                       }
            else fs
      fs'' = if open /= Ftd.None
             then fs' { Fsd.prevOpen = if open == Ftd.Buy
                                       then Ta.makeValidLeafDataMapInc fto ftado
                                       else if open == Ftd.Sell
                                            then Ta.makeValidLeafDataMapDec fto ftado
                                            else ([], M.empty)
                      }
              else fs'
      td' = td { Ftd.chart     = cd
               , Ftd.fxSetting = fs''
               , Ftd.tradeRate = if open == Ftd.Buy
                                 then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                          , Fcd.close = position
                                                          }
                                 else if open == Ftd.Sell
                                      then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                               , Fcd.close = position
                                                               }
                                      else if close /= Ftd.None
                                           then Fcd.initFxChartData
                                           else Ftd.tradeRate td
               , Ftd.side  = if open == Ftd.Buy
                             then Ftd.Buy
                             else if open == Ftd.Sell
                                  then Ftd.Sell
                                  else if close /= Ftd.None
                                       then Ftd.None
                                       else Ftd.side td
               , Ftd.trSuccess  = if close /= Ftd.None && 0 < profits 
                                  then Ftd.trSuccess td + 1
                                  else Ftd.trSuccess td
               , Ftd.trFail     = if close /= Ftd.None && profits <= 0
                                  then Ftd.trFail td + 1
                                  else Ftd.trFail td
               , Ftd.profit     = Ftd.profit td + profits
               , Ftd.realizedPL = if close /= Ftd.None
                                  then unrealizedPL
                                  else Ftd.realizedPL td
               }
      td'' = td' { Ftd.unrealizedPL = if Ftd.side td' == Ftd.Buy
                                      then Ftd.realizedPL td' + 25 * f1 td' chartLow * (chartLow / (Fcd.close $ Ftd.tradeRate td') - 1)
                                      else if Ftd.side td' == Ftd.Sell
                                           then Ftd.realizedPL td' + 25 * f1 td' chartHigh * (1 - (chartHigh / (Fcd.close $ Ftd.tradeRate td')))
                                           else Ftd.realizedPL td'
                 }
  in (open, close, td'')

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
  let ftado'  = M.map (L.dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftado
      ftadcp' = M.map (L.dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftadcp
      ftadcl' = M.map (L.dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftadcl
      ctd = Fad.FxChartTaData { Fad.taChart     = x
                              , Fad.open        = M.map (\y -> if L.null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else L.head y) ftado'
                              , Fad.closeProfit = M.map (\y -> if L.null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else L.head y) ftadcp'
                              , Fad.closeLoss   = M.map (\y -> if L.null y
                                                               then Fad.initFxTechnicalAnalysisData
                                                               else L.head y) ftadcl'
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
  let (chart, xs') = L.break (\x -> Fcd.no x `mod` c == 0) xs
  in if L.null xs'
     then let fcd  = L.head chart
          in [fcd]
     else L.head xs' : makeSimChart c (L.tail xs')

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
  in makeChartTa (L.take chartLength $ L.reverse xcd) ftado ftadcp ftadcl []


backTest :: Int ->
            V.Vector Fcd.FxChartData ->
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            (Fsd.FxSettingData, Ftd.FxTradeData)
backTest n fc td fsd =
  let ltt = Ta.getLearningTestTime fsd * Gsd.learningTestCount Gsd.gsd
      fc' = V.toList $ V.slice (n - Ta.getPrepareTimeAll fsd) (Ta.getPrepareTimeAll fsd) fc V.++ V.slice n ltt fc
      ctdl = makeChart fsd ltt fc'
      td1 = td { Ftd.fxSetting = Fsd.fxSetting fsd
               }
      td4 = L.foldl (\td2 ctd -> let (_, _, td3) = evaluate ctd fsd getQuantityBacktest False td2
                                 in td3)
            td1 ctdl
  in checkAlgoSetting ltt fsd td4

printDebug :: [Fad.FxChartTaData] -> (Fsd.FxSettingData, Ftd.FxTradeData) -> (Fsd.FxSettingData, Ftd.FxTradeData)
printDebug ctdl r =
  let a = L.map (\ctd -> (Fcd.no . Fad.chart $ (Fad.open ctd M.! 0), Fad.short . Fad.rci $ (Fad.open ctd M.! 0))) ctdl
  in traceShow(a) $ r

checkAlgoSetting :: Int ->
                    Fsd.FxSettingData ->
                    Ftd.FxTradeData ->
                    (Fsd.FxSettingData, Ftd.FxTradeData)
checkAlgoSetting l fsd td =
  let td' = td { Ftd.chartLength = l
               , Ftd.fxSetting = (Ftd.fxSetting td)
                                 { Fsd.fxTaOpen        = Ta.checkAlgoSetting . Fsd.fxTaOpen        $ Ftd.fxSetting td
                                 , Fsd.fxTaCloseProfit = Ta.checkAlgoSetting . Fsd.fxTaCloseProfit $ Ftd.fxSetting td
                                 , Fsd.fxTaCloseLoss   = Ta.checkAlgoSetting . Fsd.fxTaCloseLoss   $ Ftd.fxSetting td
                                 }
               }
      fsd' = fsd { Fsd.fxSetting = Ftd.fxSetting td'
                 }
  in (fsd', td')
  
learning :: Int -> Fsd.FxSettingData -> IO [Ftd.FxTradeData]
learning n fsd =
  R.mapM (\_ -> do let td = initFxTradeData Ftd.Backtest
                       ltt = Ta.getLearningTestTime fsd
                       fc = Fsd.chart fsd
                   n' <- getRandomR(n - ltt * Gsd.learningTestCount Gsd.gsd, n)
                   let fc' = V.toList $ V.slice (n' - (Ta.getPrepareTimeAll fsd + ltt)) (Ta.getPrepareTimeAll fsd + ltt) fc
                   -- traceShow(ltt, n, n', length fc) $ return ()
                       ctdl = makeChart fsd ltt fc'
                       (_, _, td'') = L.foldl (\(_, _, td') ctd -> evaluate ctd fsd getQuantityLearning False td') (Ftd.None, Ftd.None, td) $ L.init ctdl
                       (_, _, td''') = evaluate (L.last ctdl) fsd getQuantityLearning True td''
                   return (td''' { Ftd.chartLength = ltt })) [1 .. Gsd.learningTestCount Gsd.gsd]

trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         Fcd.FxChartData ->
         IO (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
trade td fsd e = do
  fc <- (L.++) <$> Fm.getChartListBack (Fcd.no e - 1) (Ta.getPrepareTimeAll fsd) <*> pure [e]
  let ctdl = makeChart fsd 1 fc
  return $ evaluate (L.last ctdl) fsd getQuantityBacktest False td

gaLearningEvaluate :: Int -> (Fsd.FxSettingData, Rational) -> IO (Fsd.FxSettingData, Rational)
gaLearningEvaluate n (fsd, p) = do
  p' <- (toRational . Ftd.getEvaluationValueList) <$> learning n fsd
  if p < p'
    then return (fsd, p')
    else return (fsd, p)


