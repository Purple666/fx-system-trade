module FxTrade ( initFxTradeDataBacktest
               , initFxTradeDataTrade
               , backTest
               , gaLearningEvaluate
               , trade
               , learningEvaluate
               , evaluationOk
               , getChart
               , getEvaluationValue
               ) where

import           System.Environment
import           Control.Monad
import qualified Control.Monad.Random    as R
import qualified Data.List               as L
import qualified Data.Map                as M
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxRedis                 as Fr
import qualified FxPrint                 as Fp
import qualified FxSettingData           as Fsd
import qualified FxTechnicalAnalysis     as Ta
import qualified FxTechnicalAnalysisData as Fad
import qualified FxTradeData             as Ftd
import qualified Ga
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

getEvaluationValue :: Fsd.FxSettingData -> Ftd.FxTradeData -> Double
getEvaluationValue fsd td =
  (Fsd.getLogProfit fsd * Ftd.profit td * Ftd.realizedPL td * Ftd.getWinRatePure td ^ 4) / fromIntegral (Ftd.chartLength td)

getGaEvaluationValue :: Fsd.FxSettingData -> Ftd.FxTradeData -> Double
getGaEvaluationValue fsd td = 
  (Fsd.getLogProfit fsd * Ftd.profit td * Ftd.realizedPL td * Ftd.getWinRatePure td ^ 4) / fromIntegral (Ftd.chartLength td)

evaluationOk :: Ftd.FxTradeData -> Fsd.FxSettingData -> Bool
evaluationOk td fsd =
  0 < getEvaluationValue fsd td &&
  0 < (Ftd.profit . Fsd.resultFxTradeData $ Fsd.fxSettingChart fsd) &&
  (fromIntegral $ Fsd.getLearningTestTimes fsd) * (Ftd.profit . Fsd.resultFxTradeData $ Fsd.fxSettingChart fsd) < Ftd.profit td
  
getUnitBacktest :: Ftd.FxTradeData -> Double -> Int
getUnitBacktest td chart = let u = truncate (25 * (Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd) / chart)
                           in if Ftd.maxUnit td `div` 2 < u
                              then Ftd.maxUnit td `div` 2
                              else u

getUnitLearning :: Ftd.FxTradeData -> Double -> Int
getUnitLearning td chart = getUnitBacktest td chart

{-
getUnitLearning :: Ftd.FxTradeData -> Double -> Int
getUnitLearning td chart = truncate $ (25 * Ftd.realizedPL td) / chart
-}

evaluateProfitInc :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitInc fts ftad =
  Tr.evaluateTree fst (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

evaluateProfitDec :: Fad.FxTechnicalAnalysisSetting -> M.Map Int Fad.FxTechnicalAnalysisData -> Bool
evaluateProfitDec fts ftad =
  Tr.evaluateTree snd (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)

initFxTradeDataBacktest :: Ftd.FxTradeData
initFxTradeDataBacktest =
  Ftd.initFxTradeDataCommon { Ftd.maxUnit     = Gsd.productionMaxUnit Gsd.gsd
                            , Ftd.coName      = "backtest"
                            , Ftd.environment = Ftd.Backtest
                            , Ftd.bearer      = ""
                            , Ftd.url         = ""
                            }

initFxTradeDataTrade :: Ftd.FxEnvironment -> IO (Ftd.FxTradeData)
initFxTradeDataTrade Ftd.Practice = do
  bearer <- getEnv "TRADE_PRACTICE_BEARER"
  url <- getEnv "TRADE_PRACTICE_URL"
  return $ Ftd.initFxTradeDataCommon { Ftd.maxUnit     = Gsd.practiceMaxUnit Gsd.gsd
                                     , Ftd.coName      = "trade_practice"
                                     , Ftd.environment = Ftd.Practice
                                     , Ftd.bearer      = bearer
                                     , Ftd.url         = url
                                     }
initFxTradeDataTrade Ftd.Production = do
  bearer <- getEnv "TRADE_PRODUCTION_BEARER"
  url <- getEnv "TRADE_PRODUCTION_URL"
  return $ Ftd.initFxTradeDataCommon { Ftd.maxUnit     = Gsd.productionMaxUnit Gsd.gsd
                                     , Ftd.coName      = "trade_production"
                                     , Ftd.environment = Ftd.Production
                                     , Ftd.bearer      = bearer
                                     , Ftd.url         = url
                                     }

evaluateOne :: Fad.FxChartTaData ->
               Fsd.FxSettingData ->
               (Ftd.FxTradeData -> Double -> Int) ->
               Bool ->
               Ftd.FxTradeData ->
               Fsd.FxSetting ->
               (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData, Fsd.FxSetting)
evaluateOne ctd fsd f1 forceSell td fs =
  let cd        = Fad.taChart ctd
      chartBuy  = Fcd.close cd + Gsd.spread Gsd.gsd / 2
      chartSell = Fcd.close cd - Gsd.spread Gsd.gsd / 2
      chart     = Fcd.close cd
      tradeRate = Fcd.close $ Ftd.tradeRate td
      tradeDate = Fcd.no cd - Fcd.no (Ftd.tradeRate td)
      ftado     = Fad.open        ctd
      ftadcp    = Fad.closeProfit ctd
      ftadcl    = Fad.closeLoss   ctd
      fto       = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp      = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl      = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      lcd = Gsd.maxTradeTime Gsd.gsd
      (position, open)
        | (Ftd.side td == Ftd.None || ({- Ta.getHoldTime fsd < tradeDate && -} Ftd.side td == Ftd.Sell)) &&
          evaluateProfitInc fto ftado = (chartBuy, Ftd.Buy)
        | (Ftd.side td == Ftd.None || ({- Ta.getHoldTime fsd < tradeDate && -} Ftd.side td == Ftd.Buy))  &&
          evaluateProfitDec fto ftado = (chartSell, Ftd.Sell)
        | otherwise = (0, Ftd.None)
      (profits, close)
        | open /= Ftd.None && Ftd.side td == Ftd.Buy  = (chartSell - tradeRate, Ftd.Buy)
        | open /= Ftd.None && Ftd.side td == Ftd.Sell = (tradeRate - chartBuy, Ftd.Sell)
        | Ftd.side td == Ftd.Buy &&
          (forceSell || lcd < tradeDate ||
           (0 < chartSell - tradeRate && -- Ta.getHoldTime fsd < tradeDate &&
            evaluateProfitDec ftcp ftadcp) ||
           (chartSell - tradeRate < 0 && -- Ta.getHoldTime fsd < tradeDate &&
            evaluateProfitDec ftcl ftadcl)) = (chartSell - tradeRate, Ftd.Buy)
        | Ftd.side td == Ftd.Sell &&
          (forceSell || lcd < tradeDate ||
            (0 < tradeRate - chartBuy && -- Ta.getHoldTime fsd < tradeDate &&
             evaluateProfitInc ftcp ftadcp) ||
            (tradeRate - chartBuy < 0 && -- Ta.getHoldTime fsd < tradeDate &&
             evaluateProfitInc ftcl ftadcl)) = (tradeRate - chartBuy, Ftd.Sell)
        | otherwise = (0, Ftd.None)
      fs' = if close /= Ftd.None
            then let ls  = Fsd.learningSetting fs
                     ls' = ls { Fsd.totalTradeDate     = Fsd.totalTradeDate ls + tradeDate
                              , Fsd.numTraderadeDate   = Fsd.numTraderadeDate ls + 1
                              }
                     alcOpen = Ta.calcFxalgorithmListCount profits $ Fsd.prevOpen fs
                     alcCloseProfit
                       | open == Ftd.None && close == Ftd.Buy  && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapDec ftcp ftadcp
                       | open == Ftd.None && close == Ftd.Sell && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapInc ftcp ftadcp
                       | otherwise         = (Tr.emptyLeafDataMap, M.empty)
                     alcCloseLoss
                       | open == Ftd.None && close == Ftd.Buy  && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapDec ftcl ftadcl
                       | open == Ftd.None && close == Ftd.Sell && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapInc ftcl ftadcl
                       | otherwise          = (Tr.emptyLeafDataMap, M.empty)
                     fxTaOpen        = Ta.updateAlgorithmListCount Fad.open
                                       ctd alcOpen        $ Fsd.fxTaOpen fs
                     fxTaCloseProfit = Ta.updateAlgorithmListCount Fad.closeProfit
                                       ctd alcCloseProfit $ Fsd.fxTaCloseProfit fs
                     fxTaCloseLoss   = Ta.updateAlgorithmListCount Fad.closeLoss
                                       ctd alcCloseLoss   $ Fsd.fxTaCloseLoss fs
                 in fs { Fsd.learningSetting  = ls'
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
      td' = td { Ftd.chart = cd
               , Ftd.unit  = if open /= Ftd.None
                             then f1 td . Fcd.close $ Ftd.tradeRate td'
                             else if close /= Ftd.None
                                  then 0
                                  else Ftd.unit td
               , Ftd.tradeRate = if open == Ftd.Buy
                                 then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                          , Fcd.close = position
                                                          }
                                 else if open == Ftd.Sell
                                      then Fcd.initFxChartData { Fcd.no  = Fcd.no cd
                                                               , Fcd.close = position
                                                               }
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
                                  then Ftd.realizedPL td + fromIntegral (Ftd.unit td) * profits
                                  else Ftd.realizedPL td
               }
  in (open, close, td', fs'')

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
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            IO (Fsd.FxSettingData, Ftd.FxTradeData)
backTest n td fsd = do
  let ltt = Ta.getLearningTestTime fsd * Fsd.getLearningTestCount fsd
  fc <- Fr.getChartList (n - Ta.getPrepareTimeAll fsd) (Ta.getPrepareTimeAll fsd + ltt)
  let ctdl = makeChart fsd ltt fc
      fs = Fsd.fxSetting fsd
      (td4, fs4) = L.foldl (\(td2, fs2) ctd -> let (_, _, td3, fs3) = evaluateOne ctd fsd getUnitBacktest False td2 fs2
                                               in (td3, fs3))
                   (td, fs) ctdl
  checkAlgoSetting ltt fsd td4 fs4

checkAlgoSetting :: R.MonadRandom m =>
                    Int ->
                    Fsd.FxSettingData ->
                    Ftd.FxTradeData ->
                    Fsd.FxSetting ->
                    m (Fsd.FxSettingData, Ftd.FxTradeData)
checkAlgoSetting l fsd td fs = do
  let td' = td { Ftd.chartLength = l
               }
  tao  <- Ta.checkAlgoSetting $ Fsd.fxTaOpen        fs
  tacp <- Ta.checkAlgoSetting $ Fsd.fxTaCloseProfit fs
  tacl <- Ta.checkAlgoSetting $ Fsd.fxTaCloseLoss   fs
  let fsd' = fsd { Fsd.fxSetting = fs
                                   { Fsd.fxTaOpen        = tao
                                   , Fsd.fxTaCloseProfit = tacp
                                   , Fsd.fxTaCloseLoss   = tacl
                                   }
                 }
  return (Fsd.setHashFxSettingData fsd', td')

evaluate :: Fsd.FxSettingData -> Int -> [Fcd.FxChartData] -> Ftd.FxTradeData
evaluate fsd ltt fc =
  let td = initFxTradeDataBacktest
      ctdl = makeChart fsd ltt fc
      (_, _, td2, _) = L.foldl (\(_, _, td1, _) ctd -> evaluateOne ctd fsd getUnitLearning False td1 Fsd.initFxSetting) (Ftd.None, Ftd.None, td, Fsd.initFxSetting) $ L.init ctdl
      (_, _, td3, _) = evaluateOne (L.last ctdl) fsd getUnitLearning True td2 Fsd.initFxSetting
  in if null ctdl
     then td
     else td3 { Ftd.chartLength = ltt }

getChart :: Int -> Int -> Fsd.FxSettingData -> IO (Int, [Fcd.FxChartData])
getChart n c fsd = do
  let ltt = Ta.getLearningTestTime fsd * c
      lttp = Ta.getPrepareTimeAll fsd + ltt
  fc <- Fr.getChartList (n - lttp) lttp
  return (ltt, fc)

learningEvaluate :: Int -> Fsd.FxSettingData -> IO Ftd.FxTradeData
learningEvaluate n fsd = do
  (ltt, fc) <- getChart n (Fsd.getLearningTestCount fsd) fsd
  return $ evaluate fsd ltt fc

trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         Fcd.FxChartData ->
         IO (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData, Fsd.FxSettingData)
trade td fsd e = do
  fc <- (L.++) <$> Fr.getChartList (Fcd.no e - 1 - Ta.getPrepareTimeAll fsd) (Ta.getPrepareTimeAll fsd) <*> pure [e]
  let ctdl = makeChart fsd 1 fc
      (open, close, td', fs) = evaluateOne (L.last ctdl) fsd getUnitBacktest False td (Fsd.fxSetting fsd)
  (fsd', td'') <- checkAlgoSetting 1 fsd td' fs
  return (open, close, td'', fsd')

gaLearningEvaluate :: Ga.LearningData Fsd.FxSettingData -> Ga.LearningData Fsd.FxSettingData
gaLearningEvaluate (Ga.LearningData ld) =
  Ga.LearningData $ L.map (\(fsd, _) -> let ltt = Fsd.learningTestTime $ Fsd.fxSettingChart fsd
                                            fc = Fsd.chart $ Fsd.fxSettingChart fsd
                                            td = evaluate fsd ltt fc
                                            fsd' = fsd { Fsd.fxSettingChart = (Fsd.fxSettingChart fsd) {
                                                           Fsd.resultFxTradeData = td
                                                           }
                                                       }
                                            p = toRational $ getGaEvaluationValue fsd td
                                        in (fsd', p)) ld



