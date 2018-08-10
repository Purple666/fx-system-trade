module GlobalSettingFunction where

import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxSetting                as Fs


getEvaluationValue :: Ftd.FxTradeData -> Double
getEvaluationValue x =
  if Ftd.unrealizedPL x <= Gsd.initalProperty Gsd.gsd
  then 0
  else Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x
{-  
  Ftd.profit x

  if Ftd.trTrade x == 0 || Ftd.trTradeDate x == 0
  then 0
  else Ftd.profit x * Ftd.realizedPL x / ((fromIntegral $ Ftd.trTradeDate x) / (fromIntegral $ Ftd.trTrade x))
-}

evaluationOk :: Ftd.FxTradeData -> [Ftd.FxTradeData] -> Bool
evaluationOk tdl tdlt =
  (and $ map (\x -> 0 < getEvaluationValue x) tdlt) && 0 < getEvaluationValue tdl 

getEvaluationValueList :: [Ftd.FxTradeData] -> Double
getEvaluationValueList tdlt =
  sum $ map getEvaluationValue tdlt

buyEvaluation :: Ftd.FxTradeData -> Fcd.FxChartData -> Fsd.FxSettingData -> Double -> Double -> Bool
buyEvaluation td cd fsd chart rate =
  --Ftd.side td == Ftd.None 
  Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Sell && Fs.getSimChartMax fsd < Fcd.no cd - (Fcd.no $ Ftd.rate td))
  --Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Sell && 0 < rate - chart)

sellEvaluation :: Ftd.FxTradeData -> Fcd.FxChartData -> Fsd.FxSettingData -> Double -> Double -> Bool
sellEvaluation td cd fsd chart rate =
  --Ftd.side td == Ftd.None
  Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Buy && Fs.getSimChartMax fsd < Fcd.no cd - (Fcd.no $ Ftd.rate td))
  --Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Buy  && 0 < chart - rate )

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = if ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25 < (Ftd.realizedPL td) / Gsd.quantityRate Gsd.gsd
                               then ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityBacktest = getQuantityLearning

{-
getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest _ _ = (Gsd.initalProperty Gsd.gsd) / (Gsd.quantityRate Gsd.gsd)

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning = getQuantityBacktest 
-}


-- ===============================================================================================

instance Eq Ftd.FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord Ftd.FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT


