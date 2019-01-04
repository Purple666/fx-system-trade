module FxTrade ( initFxTradeData
               , backTest
               , learning
               , trade
               , gaLearningEvaluate
               , evaluationOk
               , evaluationOk2
               , getProfitList
               ) where

import           Control.Monad
import qualified Data.Map                as M
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxMongodb               as Fm
import qualified FxPrint                 as Fp
import qualified FxSetting               as Fs
import qualified FxSettingData           as Fsd
import qualified FxTechnicalAnalysis     as Ta
import qualified FxTechnicalAnalysisData as Fad
import qualified FxTradeData             as Ftd
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

evaluationOk :: Ftd.FxTradeData -> [Ftd.FxTradeData] -> Bool
evaluationOk tdl tdlt =
  all (\x -> 0 < Ftd.getEvaluationValue x) tdlt && 0 < Ftd.getEvaluationValue tdl && 0 < Ftd.profit tdl && 0 < getProfitList tdlt 

evaluationOk2 :: Ftd.FxTradeData -> [Ftd.FxTradeData] -> Bool
evaluationOk2 tdl tdlt =
  0 < Ftd.getEvaluationValue tdl && 0 < Ftd.getEvaluationValueList tdlt && 0 < Ftd.profit tdl && 0 < getProfitList tdlt 

getProfitList :: [Ftd.FxTradeData] -> Double
getProfitList tdlt =
  sum $ map Ftd.profit tdlt

getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest td chart = if (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25 < Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd
                               then (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = Ftd.realizedPL td

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
            Int ->
            (Ftd.FxTradeData -> Double -> Double) ->
            Bool ->
            Ftd.FxTradeData ->
            (Ftd.FxSide, Ftd.FxSide, Fsd.FxSettingData, Ftd.FxTradeData)
evaluate ctd fsd plsf f1 forceSell td =
{-    
        | Ftd.side td == Ftd.None && evaluateProfitInc fto ftado = (chart, Ftd.Buy)
        | Ftd.side td == Ftd.None && evaluateProfitDec fto ftado = (chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)
        | (Ftd.side td == Ftd.None || (Fs.getTradeHoldTime fsd < Fcd.no cd - tradeNo && Ftd.side td == Ftd.Sell)) &&
          evaluateProfitInc fto ftado = (chart, Ftd.Buy)
        | (Ftd.side td == Ftd.None || (Fs.getTradeHoldTime fsd < Fcd.no cd - tradeNo && Ftd.side td == Ftd.Buy)) &&
          evaluateProfitDec fto ftado = (chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)         
-}
  
  let cd        = Fad.taChart ctd
      chart     = if Ftd.side td == Ftd.Buy
                  then Fcd.close cd - Gsd.spread Gsd.gsd
                  else if Ftd.side td == Ftd.Sell
                       then Fcd.close cd + Gsd.spread Gsd.gsd
                       else Fcd.close cd
      tradeRate = Fcd.close $ Ftd.tradeRate td
      tradeNo   = Fcd.no $ Ftd.tradeRate td
      tradeDate = Fcd.no cd - tradeNo
      ftado     = Fad.open        ctd
      ftadcp    = Fad.closeProfit ctd
      ftadcl    = Fad.closeLoss   ctd
      fto       = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp      = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl      = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      unrealizedPL
        | Ftd.side td == Ftd.Buy  = Ftd.realizedPL td + 25 * f1 td chart * ((chart / tradeRate) - 1)
        | Ftd.side td == Ftd.Sell = Ftd.realizedPL td + 25 * f1 td chart * (1 - (chart / tradeRate))
        | otherwise = Ftd.realizedPL td
      (position, open)
        | (Ftd.side td == Ftd.None ||
           (0 < tradeRate - chart && Fs.getTradeHoldTime fsd < tradeDate && Ftd.side td == Ftd.Sell)) &&
          evaluateProfitInc fto ftado = (chart, Ftd.Buy)
        | (Ftd.side td == Ftd.None ||
           (0 < chart - tradeRate && Fs.getTradeHoldTime fsd < tradeDate && Ftd.side td == Ftd.Buy)) &&
          evaluateProfitDec fto ftado = (chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)
      (profits, close)
        | open /= Ftd.None && Ftd.side td == Ftd.Buy  = (chart - tradeRate, Ftd.Close)
        | open /= Ftd.None && Ftd.side td == Ftd.Sell = (tradeRate - chart, Ftd.Close)
        | Ftd.side td == Ftd.Buy && (forceSell || Fs.getLearningTestTime fsd < tradeDate ||
                                     (Fs.getTradeHoldTime fsd < Fcd.no cd - tradeNo &&
                                      (0 < chart - tradeRate && evaluateProfitDec ftcp ftadcp ||
                                       chart - tradeRate < 0 && evaluateProfitDec ftcl ftadcl ||
                                       Fs.getProfitRate fsd < unrealizedPL - Ftd.realizedPL td ||
                                       unrealizedPL - Ftd.realizedPL td < Fs.getLossCutRate fsd))) = (chart - tradeRate, Ftd.Buy)
        | Ftd.side td == Ftd.Sell && (forceSell || Fs.getLearningTestTime fsd < tradeDate ||
                                      (Fs.getTradeHoldTime fsd < Fcd.no cd - tradeNo &&
                                       (0 < tradeRate - chart && evaluateProfitInc ftcp ftadcp ||
                                        tradeRate - chart < 0 && evaluateProfitInc ftcl ftadcl || 
                                        Fs.getProfitRate fsd < unrealizedPL - Ftd.realizedPL td || 
                                        unrealizedPL - Ftd.realizedPL td < Fs.getLossCutRate fsd))) = (tradeRate - chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)
      fsd' = if close /= Ftd.None
             then let ls  = Fsd.learningSetting $ Fsd.fxSetting fsd
                      ls' = ls { Fsd.trTrade         = Fsd.trTrade ls + 1
                               , Fsd.trTradeDate     = Fsd.trTradeDate ls + (fromIntegral $ tradeDate)
                               , Fsd.trSuccess       = if 0 < profits
                                                       then Fsd.trSuccess ls + 1
                                                       else Fsd.trSuccess ls
                               , Fsd.trFail          = if profits <= 0
                                                       then Fsd.trFail ls + 1
                                                       else Fsd.trFail ls
                               , Fsd.successProfit   = if 0 < profits
                                                       then Fsd.successProfit ls + profits
                                                       else Fsd.successProfit ls
                               , Fsd.failProfit      = if profits <= 0
                                                       then Fsd.failProfit ls +  profits
                                                       else Fsd.failProfit ls
                               }
                      alcOpen
                        | 0 < profits = Ta.calcFxalgorithmListCount profits $ Fsd.prevOpen fsd
                        | otherwise   = (Tr.emptyLeafDataMap, M.empty)
                      alcCloseProfit
                        | close == Ftd.Buy  && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapDec ftcp ftadcp
                        | close == Ftd.Sell && 0 < profits = Ta.calcFxalgorithmListCount profits $ Ta.makeValidLeafDataMapInc ftcp ftadcp
                        | otherwise         = (Tr.emptyLeafDataMap, M.empty)
                      alcCloseLoss
                        | close == Ftd.Buy  && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapDec ftcl ftadcl
                        | close == Ftd.Sell && profits <= 0 = Ta.calcFxalgorithmListCount (abs profits) $ Ta.makeValidLeafDataMapInc ftcl ftadcl
                        | otherwise         = (Tr.emptyLeafDataMap, M.empty)
                      fsd1 = fsd { Fsd.prevOpen = if open == Ftd.Buy
                                                  then Ta.makeValidLeafDataMapInc fto ftado
                                                  else if open == Ftd.Sell
                                                       then Ta.makeValidLeafDataMapDec fto ftado
                                                       else ([], M.empty)
                                 , Fsd.fxSetting = (Fsd.fxSetting fsd)
                                                   { Fsd.learningSetting  = ls'
                                                   , Fsd.fxTaOpen         = Ta.updateAlgorithmListCount Fad.open
                                                                            ctd alcOpen        (Fsd.fxTaOpen $ Fsd.fxSetting fsd)
                                                   , Fsd.fxTaCloseProfit  = Ta.updateAlgorithmListCount Fad.closeProfit
                                                                            ctd alcCloseProfit (Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd)
                                                   , Fsd.fxTaCloseLoss    = Ta.updateAlgorithmListCount Fad.closeLoss
                                                                            ctd alcCloseLoss   (Fsd.fxTaCloseLoss $ Fsd.fxSetting fsd)
                                                   }
                                 }
                      fslu = Fsd.fxSettingLog fsd1
                      fsl = if M.member (Fsd.no $ Fsd.fxSetting fsd1) fslu
                            then M.adjust (\(_, a, b) -> (Fsd.fxSetting fsd1, a + profits, b + 1)) (Fsd.no $ Fsd.fxSetting fsd1) fslu
                            else if plsf == 0
                                 then M.insert (Fsd.no $ Fsd.fxSetting fsd1) (Fsd.fxSetting fsd1, profits, 1) fslu
                                 else fslu
                  in fsd1 { Fsd.fxSettingLog = fsl
                          }
             else if open /= Ftd.None
                  then fsd { Fsd.prevOpen = if open == Ftd.Buy
                                            then Ta.makeValidLeafDataMapInc fto ftado
                                            else if open == Ftd.Sell
                                                 then Ta.makeValidLeafDataMapDec fto ftado
                                                 else ([], M.empty)
                           }
                  else fsd
      td' = td { Ftd.chart     = cd
               , Ftd.tradeRate = if open == Ftd.Buy || open == Ftd.Sell
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
                                      then Ftd.realizedPL td' + 25 * f1 td' chart * ((chart / (Fcd.close $ Ftd.tradeRate td')) - 1)
                                      else if Ftd.side td' == Ftd.Sell
                                           then Ftd.realizedPL td' + 25 * f1 td' chart * (1 - (chart / (Fcd.close $ Ftd.tradeRate td')))
                                           else Ftd.realizedPL td'
                 }
  in (open, close, fsd', td'')

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
  let ftado'  = M.map (dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftado
      ftadcp' = M.map (dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftadcp
      ftadcl' = M.map (dropWhile (\b -> Fcd.no x < Fcd.no (Fad.chart b))) ftadcl
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
{-
          in [fcdClose]
-}
          in if snd high < snd low
             then [fcdHigh, fcdLow, fcdClose]
             else [fcdLow,  fcdHigh, fcdClose]
     else if null chart
          then head xs' : makeSimChart c (tail xs')
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
{-
               in fcdClose : makeSimChart c (tail xs')
-}
               in if snd high < snd low
                  then fcdHigh : fcdLow : fcdClose : (makeSimChart c $ tail xs')
                  else fcdLow : fcdHigh : fcdClose : (makeSimChart c $ tail xs')

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
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            [Fcd.FxChartData] ->            
            IO (Fsd.FxSettingData, Ftd.FxTradeData)
backTest latest l plsf td fsd xcd = do
  let ctdl = makeChart fsd l xcd
  (fsd3, td3) <- foldl (\a ctd -> do (fsd1, td1) <- a
                                     let (open, close, fsd2, td2) = evaluate ctd fsd1 plsf getQuantityBacktest False td1
                                       -- Control.Monad.when (latest && (open /= Ftd.None || close /= Ftd.None)) $ Fp.printTradeResult open close td' td3 0
                                     return (fsd2, td2))
                 (pure (fsd, td)) ctdl
  return (fsd3, td3)

learning :: Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            Ftd.FxTradeData
learning td fsd =
  let fc = Fsd.fxChart fsd
      ctdl = makeChart fsd (Fsd.chartLength fc) (Fsd.chart fc)
      (_, _, _, td'') = foldl (\(_, _, _, td') ctd -> evaluate ctd fsd 0 getQuantityLearning False td')
                        (Ftd.None, Ftd.None, fsd, td) $ init ctdl
      (_, _, _, td''') = evaluate (last ctdl) fsd 0 getQuantityLearning True td''
  in if null ctdl
     then td
     else td'''


trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         [Fcd.FxChartData] ->
         IO (Ftd.FxSide, Ftd.FxSide, Fsd.FxSettingData, Ftd.FxTradeData)
trade td fsd xcd = do
  let ctdl = makeChart fsd 1 xcd
      (open, close, fsd', td') =  evaluate (last ctdl) fsd 0 getQuantityBacktest False td
  return (open, close, fsd', td')

gaLearningEvaluate :: Fsd.FxSettingData -> (Fsd.FxSettingData, Rational)
gaLearningEvaluate fsd =
  let td = learning (initFxTradeData Ftd.Backtest) fsd
  in (fsd, toRational $ Ftd.getEvaluationValue td)


