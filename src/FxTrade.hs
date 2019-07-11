module FxTrade ( initFxTradeData
               , backTest
               , learning
               , trade
               , gaLearningEvaluate
               , evaluationOk
               ) where

import           Control.Monad
import qualified Data.Map                as M
import           Debug.Trace
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
  (and $ map (\x -> 0 < Ftd.getEvaluationValue x) tdlt) && (and $ map (\x -> Gsd.initalProperty Gsd.gsd  < Ftd.realizedPL x) tdlt)

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
      tradeRate = Fcd.close $ Ftd.tradeRate td
      tradeDate = Fcd.no cd - (Fcd.no $ Ftd.tradeRate td)
      ftado     = Fad.open        ctd
      ftadcp    = Fad.closeProfit ctd
      ftadcl    = Fad.closeLoss   ctd
      fto       = Fsd.fxTaOpen        $ Fsd.fxSetting fsd
      ftcp      = Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
      ftcl      = Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
      fs        = Ftd.fxSetting td
      lcd = Ta.getLearningTestTime fsd
      unrealizedPL
        | Ftd.side td == Ftd.Buy  = Ftd.realizedPL td + 25 * f1 td chart * ((chart / tradeRate) - 1)
        | Ftd.side td == Ftd.Sell = Ftd.realizedPL td + 25 * f1 td chart * (1 - (chart / tradeRate))
        | otherwise = Ftd.realizedPL td
      (position, open)
        | (Ftd.side td == Ftd.None ||
           (Ftd.side td == Ftd.Sell && (0.01 <= tradeRate - (chart + Gsd.spread Gsd.gsd / 2) ||
                                        tradeRate - (chart + Gsd.spread Gsd.gsd / 2)<= -0.01))) &&
          evaluateProfitInc fto ftado = (chart + Gsd.spread Gsd.gsd / 2, Ftd.Buy)
        | (Ftd.side td == Ftd.None ||
           (Ftd.side td == Ftd.Buy && (0.01 <= chart - Gsd.spread Gsd.gsd / 2 - tradeRate ||
                                       chart - Gsd.spread Gsd.gsd / 2  - tradeRate <= -0.01))) &&
           evaluateProfitDec fto ftado = (chart - Gsd.spread Gsd.gsd / 2, Ftd.Sell)
        | otherwise = (0, Ftd.None)
{-
        | Ftd.side td == Ftd.None && evaluateProfitInc fto ftado = (chart, Ftd.Buy)
        | Ftd.side td == Ftd.None && evaluateProfitDec fto ftado = (chart, Ftd.Sell)
        | otherwise = (0, Ftd.None)
-}        
      (profits, close)
        | open /= Ftd.None && Ftd.side td == Ftd.Buy  = (chart - Gsd.spread Gsd.gsd / 2 - tradeRate, Ftd.Buy)
        | open /= Ftd.None && Ftd.side td == Ftd.Sell = (tradeRate - (chart + Gsd.spread Gsd.gsd / 2), Ftd.Sell)
        | Ftd.side td == Ftd.Buy &&
          (forceSell || lcd < tradeDate ||
            (0.01 <= chart - Gsd.spread Gsd.gsd / 2 - tradeRate  && evaluateProfitDec ftcp ftadcp) ||
            (chart - Gsd.spread Gsd.gsd / 2 - tradeRate <= -0.01 && evaluateProfitDec ftcl ftadcl)) =
            (chart - Gsd.spread Gsd.gsd / 2 - tradeRate, Ftd.Buy)
        | Ftd.side td == Ftd.Sell &&
          (forceSell || lcd < tradeDate ||
            (0.01 <= tradeRate - (chart  + Gsd.spread Gsd.gsd / 2) && evaluateProfitInc ftcp ftadcp) ||
            (tradeRate - (chart  + Gsd.spread Gsd.gsd / 2) <= -0.01 && evaluateProfitInc ftcl ftadcl)) =
            (tradeRate - (chart + Gsd.spread Gsd.gsd / 2), Ftd.Sell)
        | otherwise = (0, Ftd.None)
      fs' = if close /= Ftd.None
            then let ls  = Fsd.learningSetting fs
                     ls' = ls { Fsd.trTrade         = Fsd.trTrade ls + 1
                              , Fsd.trTradeDate     = Fsd.trTradeDate ls + (fromIntegral $ tradeDate)
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
                                      then Ftd.realizedPL td' + 25 * f1 td' chart * ((chart / (Fcd.close $ Ftd.tradeRate td')) - 1)
                                      else if Ftd.side td' == Ftd.Sell
                                           then Ftd.realizedPL td' + 25 * f1 td' chart * (1 - (chart / (Fcd.close $ Ftd.tradeRate td')))
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
     then let fcd  = head chart
          in [fcd]
     else head xs' : makeSimChart c (tail xs')

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

backTest :: Int ->
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            [Fcd.FxChartData] ->            
            (Fsd.FxSettingData, Ftd.FxTradeData)
backTest l td fsd xcd =
  let ctdl = makeChart fsd l xcd
      td1 = td { Ftd.fxSetting = Fsd.fxSetting fsd
               }
      td4 = foldl (\td2 ctd -> let (_, _, td3) = evaluate ctd fsd getQuantityBacktest False td2
                               in td3)
            td1 ctdl
  in {- traceShow(l) $ printDebug ctdl $ -} checkAlgoSetting l fsd td4

printDebug :: [Fad.FxChartTaData] -> (Fsd.FxSettingData, Ftd.FxTradeData) -> (Fsd.FxSettingData, Ftd.FxTradeData)
printDebug ctdl r =
  let a = map (\ctd -> (Fcd.no . Fad.chart $ (Fad.open ctd M.! 0), Fad.short . Fad.rci $ (Fad.open ctd M.! 0))) ctdl
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
  
learning :: Fsd.FxSettingData ->
            [Ftd.FxTradeData]
learning fsd =
  map (\fc -> let td = initFxTradeData Ftd.Backtest
                  ctdl = makeChart fsd (Fsd.chartLength fc) (Fsd.chart fc)
                  (_, _, td'') = foldl (\(_, _, td') ctd -> evaluate ctd fsd getQuantityLearning False td') (Ftd.None, Ftd.None, td) $ init ctdl
                  (_, _, td''') = evaluate (last ctdl) fsd getQuantityLearning True td''
              in td''' { Ftd.chartLength = Fsd.chartLength fc }) $ Fsd.fxChart fsd

trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         [Fcd.FxChartData] ->
         (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
trade td fsd xcd =
  let ctdl = makeChart fsd 1 xcd
      (open, close, td') = evaluate (last ctdl) fsd getQuantityBacktest False td
  in (open, close, td')

gaLearningEvaluate :: Fsd.FxSettingData -> (Fsd.FxSettingData, Rational)
gaLearningEvaluate fsd =
  (fsd, toRational . Ftd.getEvaluationValueList $ learning fsd)


