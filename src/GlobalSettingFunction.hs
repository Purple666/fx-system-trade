module GlobalSettingFunction where

import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd
import qualified FxTechnicalAnalysisData  as Fad
import qualified FxTechnicalAnalysis      as Ta

getEvaluationValue :: Ftd.FxTradeData -> Double
getEvaluationValue x =
  if Ftd.profit x < 0 && Ftd.realizedPL x < 0
  then - Ftd.profit x * (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x ^ (2 :: Int)
  else   Ftd.profit x * (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x ^ (2 :: Int)
 {-
  if Ftd.profit x < 0 && Ftd.realizedPL x < 0
  then - Ftd.profit x * (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x ^ (4 :: Int)
  else   Ftd.profit x * (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * Ftd.getWinRatePure x ^ (4 :: Int)
  profit x * getWinRatePure x ^ (4 :: Int)
  profit x * (realizedPL x / Gsd.initalProperty Gsd.gsd) * (logBase 10 . fromIntegral $ trSuccess x) * getWinRatePure x ^ 4
-}

buyEvaluation :: Ftd.FxTradeData -> Double -> Double -> Bool
buyEvaluation td chart rate =
  --Ftd.side td == Ftd.None 
  --Ftd.side td == Ftd.None || Ftd.side td == Ftd.Sell
  Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Sell && 0 < rate - chart)

sellEvaluation :: Ftd.FxTradeData -> Double -> Double -> Bool
sellEvaluation td chart rate =
  --Ftd.side td == Ftd.None
  --Ftd.side td == Ftd.None || Ftd.side td == Ftd.Buy
  Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Buy  && 0 < chart - rate )

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = if ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25 < (Ftd.realizedPL td) / Gsd.quantityRate Gsd.gsd
                               then ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest _ _ = (Gsd.initalProperty Gsd.gsd) / (Gsd.quantityRate Gsd.gsd)

-- ===============================================================================================

instance Eq Ftd.FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord Ftd.FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT


