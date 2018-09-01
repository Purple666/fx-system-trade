module GlobalSettingFunction where

import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxSetting                as Fs

getEvaluationValue :: Ftd.FxTradeData -> Double
getEvaluationValue x =
  if Ftd.trTrade x == 0 || Ftd.trTradeDate x == 0 -- || Ftd.trFail x < 2
  then 0
  else Ftd.profit x * (Ftd.unrealizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x ^ 4 -- / ((fromIntegral $ Ftd.trTradeDate x) / (fromIntegral $ Ftd.trTrade x))
{-  
  Ftd.profit x

if Ftd.unrealizedPL x <= Gsd.initalProperty Gsd.gsd
  then 0
  else Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x

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


