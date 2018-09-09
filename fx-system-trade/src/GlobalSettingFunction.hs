module GlobalSettingFunction where

import qualified FxChartData       as Fcd
import qualified FxSetting         as Fs
import qualified FxSettingData     as Fsd
import qualified FxTradeData       as Ftd
import qualified GlobalSettingData as Gsd

getEvaluationValue :: Ftd.FxTradeData -> Double
getEvaluationValue x =
  Ftd.profit x / Ftd.getWinRatePure x ^ 4
{-
  if Ftd.unrealizedPL x < Gsd.initalProperty Gsd.gsd && Ftd.profit x < 0
  then - Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd)
  else   Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd)
  if Ftd.unrealizedPL x < Gsd.initalProperty Gsd.gsd && Ftd.profit x < 0
  then - (fromIntegral $ Ftd.trFail x + 1) ^ 4 * Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd)
  else   (fromIntegral $ Ftd.trFail x + 1) ^ 4 * Ftd.profit x * (Ftd.unrealizedPL x - Gsd.initalProperty Gsd.gsd)
  
if Ftd.trTrade x == 0 || Ftd.trTradeDate x == 0                                                                                         
  then 0                                                                                                                                  
  else (Ftd.trFail x + 1) * Ftd.profit x * (Ftd.unrealizedPL x / Gsd.initalProperty Gsd.gsd) / (fromIntegral (Ftd.trTradeDate x) / fromIntegral (Ftd.trTrade x)

)                                                                                                              

  if Ftd.trTrade x == 0 || Ftd.trTradeDate x == 0
  then 0
  else Ftd.profit x * (Ftd.unrealizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x -- / (fromIntegral (Ftd.trTradeDate x) / fromIntegral (Ftd.trTrade x))
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
  all (\x -> 0 < getEvaluationValue x) tdlt && 0 < getEvaluationValue tdl

getEvaluationValueList :: [Ftd.FxTradeData] -> Double
getEvaluationValueList tdlt =
  sum $ map getEvaluationValue tdlt

{-
getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest _ _ = (Gsd.initalProperty Gsd.gsd) / (Gsd.quantityRate Gsd.gsd)

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning = getQuantityBacktest
-}

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = if (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25 < Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd
                               then (fromIntegral (Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityBacktest = getQuantityLearning



-- ===============================================================================================

instance Eq Ftd.FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord Ftd.FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT


