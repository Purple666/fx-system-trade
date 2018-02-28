module FxTrade ( backTest
               , learning
               , trade
               , gaLearningEvaluate
               ) where
  
import Debug.Trace
import qualified Data.Map.Lazy            as M
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxTechnicalAnalysisData  as Fad
import qualified FxTradeData              as Ftd
import qualified FxTechnicalAnalysis      as Ta
import qualified GlobalSettingData        as Gsd

{-

getQuantity :: Ftd.FxTradeData -> Double -> Double
getQuantity td chart = if ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25 < (Ftd.realizedPL td) / Gsd.quantityRate Gsd.gsd
                       then ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25
                       else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

-}
getQuantity :: Ftd.FxTradeData -> Double -> Double
getQuantity _ _ = (Gsd.initalProperty Gsd.gsd) / (Gsd.quantityRate Gsd.gsd)

evaluate :: Fad.FxChartTaData -> 
            Fsd.FxSettingData ->
            Bool ->
            Ftd.FxTradeData ->
            (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
evaluate ctd fsd forceSell td = do
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
        = if (Ftd.side td == Ftd.None ||
              (Ftd.side td == Ftd.Sell && 0 < rate - chart)) &&
             Fad.evaluateProfitInc fto ftado
          then (-Gsd.spread Gsd.gsd, chart, Ftd.Buy)
          else if (Ftd.side td == Ftd.None ||
                   (Ftd.side td == Ftd.Buy && 0 < chart - rate)) &&
                  Fad.evaluateProfitDec fto ftado
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
                        (0 < chart - rate  && Fad.evaluateProfitDec ftcp ftadcp) ||
                        (chart - rate < 0  && Fad.evaluateProfitDec ftcl ftadcl) ||
                         24 * 60 * 5 * 4 * 3 < Fcd.date cd - (Fcd.date $ Ftd.rate td))
                    then (chart - rate, (chart / rate) - 1, Ftd.Buy)
                    else if Ftd.side td == Ftd.Sell &&
                            (forceSell || 
                             (0 < rate - chart  && Fad.evaluateProfitInc ftcp ftadcp) ||
                              (rate - chart < 0 && Fad.evaluateProfitInc ftcl ftadcl) ||
                              24 * 60 * 5 * 4 * 3 < Fcd.date cd - (Fcd.date $ Ftd.rate td))
                         then (rate - chart, 1 - (chart / rate), Ftd.Sell)
                         else (0, 0, Ftd.None)
               else (0, 0, Ftd.None)
      td' = td { Ftd.chart  = cd
               , Ftd.rate   = if open == Ftd.Buy
                              then Fcd.FxChartData { Fcd.date  = Fcd.date cd
                                                   , Fcd.close = position + Gsd.spread Gsd.gsd
                                                   }
                              else if open == Ftd.Sell
                                   then Fcd.FxChartData { Fcd.date  = Fcd.date cd
                                                        , Fcd.close = position - Gsd.spread Gsd.gsd
                                                        }
                                   else if close /= Ftd.None
                                        then 0
                                        else Ftd.rate td
               , Ftd.alcOpen =
                   Fad.FxalgorithmListCount { Fad.prev =
                                                if open == Ftd.Buy 
                                                then Fad.makeValidLeafDataMapInc fto ftado
                                                else if open == Ftd.Sell
                                                     then Fad.makeValidLeafDataMapDec fto ftado
                                                     else if close /= Ftd.None
                                                          then ([], M.empty)
                                                          else Fad.prev $ Ftd.alcOpen td
                                            , Fad.listCount =
                                                if close /= Ftd.None
                                                then Fad.addFxalgorithmListCount profits (Fad.prev $ Ftd.alcOpen td)
                                                     (Fad.listCount $ Ftd.alcOpen td)
                                                else Fad.listCount $ Ftd.alcOpen td
                                            }
               , Ftd.alcCloseProfit =
                   Fad.FxalgorithmListCount { Fad.prev = ([], M.empty)
                                            , Fad.listCount =
                                                if close == Ftd.Buy && 0 < profits
                                                then Fad.addFxalgorithmListCount profits (Fad.makeValidLeafDataMapDec ftcp ftadcp)
                                                     (Fad.listCount $ Ftd.alcCloseProfit td)
                                                else if close == Ftd.Sell && 0 < profits
                                                     then Fad.addFxalgorithmListCount profits (Fad.makeValidLeafDataMapInc ftcp ftadcp)
                                                          (Fad.listCount $ Ftd.alcCloseProfit td)
                                                     else Fad.listCount $ Ftd.alcCloseProfit td
                                            }
               , Ftd.alcCloseLoss =
                   Fad.FxalgorithmListCount { Fad.prev = ([], M.empty)
                                            , Fad.listCount =
                                                if close == Ftd.Buy && profits < 0
                                                then Fad.addFxalgorithmListCount (-1 / profits)
                                                     (Fad.makeValidLeafDataMapDec ftcl ftadcl)
                                                     (Fad.listCount $ Ftd.alcCloseLoss td)
                                                else if close == Ftd.Sell && profits < 0
                                                     then Fad.addFxalgorithmListCount (-1 / profits)
                                                          (Fad.makeValidLeafDataMapInc ftcl ftadcl)
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
               , Ftd.realizedPL = Ftd.realizedPL td + 25 * (getQuantity td chart) * realizedPL
               , Ftd.unrealizedPL = if (Fcd.close $ Ftd.rate td') /= 0
                                    then if Ftd.side td' == Ftd.Buy
                                         then Ftd.realizedPL td' + 25 * (getQuantity td' chart) * ((chart / (Fcd.close $ Ftd.rate td')) - 1)
                                         else if Ftd.side td' == Ftd.Sell
                                              then Ftd.realizedPL td' + 25 * (getQuantity td' chart) * (1 - (chart / (Fcd.close $ Ftd.rate td')))
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
  let ftado' = M.map (\a -> dropWhile (\b -> Fcd.date x < (Fcd.date $ Fad.chart b)) a) ftado
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
      ftado  = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fsd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaOpen fs
      ftadcp = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fsd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseProfit fs
      ftadcl = M.map (\x -> Ta.makeFxTechnicalAnalysisDataList x [] (Fsd.makeSimChart (Fad.simChart x) xcd) [])
               . Fad.algoSetting $ Fsd.fxTaCloseLoss fs
  in makeChartTa (take chartLength $ reverse xcd) ftado ftadcp ftadcl []

backTest :: Int ->
            Int ->
            Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            [Fcd.FxChartData] ->
            (Ftd.FxTradeData, [Fad.FxChartTaData])
backTest l s td fsd xcd =
  let ctdl = makeChart fsd l xcd
      (_, _, td'') = foldl (\(_, _, td') ctd -> if 0 < s && Ftd.trSuccess td + (s - 1) < Ftd.trSuccess td'
                                                then (Ftd.None, Ftd.None, td')
                                                else evaluate ctd fsd False td') (Ftd.None, Ftd.None, td) ctdl
  in (td'', ctdl)
        
learning :: Ftd.FxTradeData ->
            Fsd.FxSettingData ->
            Ftd.FxTradeData
learning td fsd = 
  let fc = Fsd.fxChart fsd
      ctdl = makeChart fsd (Fsd.chartLength fc) (Fsd.chart fc)
      (_, _, td'') = foldl (\(_, _, td') ctd -> evaluate ctd fsd False td')
                     (Ftd.None, Ftd.None, td) $ init ctdl
      (_, _, td''') = evaluate (last ctdl) fsd True td''
  in if null ctdl
     then td
     else td'''
          
trade :: Ftd.FxTradeData ->
         Fsd.FxSettingData ->
         [Fcd.FxChartData] ->
         (Ftd.FxSide, Ftd.FxSide, Ftd.FxTradeData)
trade td fsd xcd = 
  let ctdl = makeChart fsd 1 xcd
  in evaluate (head ctdl) fsd False td
  
gaLearningEvaluate :: Fsd.FxSettingData -> (Fsd.FxSettingData, Rational)
gaLearningEvaluate fsd =
  let td = learning (Ftd.initFxTradeData Ftd.Backtest) fsd
  in (fsd, toRational $ Ftd.getEvaluationValue td)

  
