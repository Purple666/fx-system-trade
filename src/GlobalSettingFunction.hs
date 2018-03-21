module GlobalSettingFunction where

import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd

getEvaluationValue :: Ftd.FxTradeData -> Double
getEvaluationValue x = 
  if Ftd.realizedPL x < 0
  then (1 / Gsd.initalProperty Gsd.gsd) 
  else (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd)
 {-
  if Ftd.realizedPL x < 0
  then (1 / Gsd.initalProperty Gsd.gsd) * (1 + Ftd.getWinRatePure x) 
  else if 0 < Ftd.trSuccess x && Gsd.initalProperty Gsd.gsd < Ftd.realizedPL x  
       then (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * (1 + Ftd.getWinRatePure x ^ 4) * (1 + (logBase 100 . fromIntegral $ Ftd.trSuccess x))
       else (Ftd.realizedPL x / Gsd.initalProperty Gsd.gsd) * (1 + Ftd.getWinRatePure x ^ 4)

-}

evaluationOk :: Ftd.FxTradeData -> [Ftd.FxTradeData] -> Bool
evaluationOk tdl tdlt =
  (and $ map (\x -> 0 < Ftd.profit x) tdlt) && 0 < Ftd.profit tdl 
  --0 < (Ftd.profit $ sum tdlt) && 0 < Ftd.profit tdl 

getEvaluationValueList :: [Ftd.FxTradeData] -> Double
getEvaluationValueList tdlt =
  Ftd.profit $ sum tdlt

buyEvaluation :: Ftd.FxTradeData -> Double -> Double -> Bool
buyEvaluation td chart rate =
  Ftd.side td == Ftd.None 
  --Ftd.side td == Ftd.None || Ftd.side td == Ftd.Sell
  --Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Sell && 0 < rate - chart)

sellEvaluation :: Ftd.FxTradeData -> Double -> Double -> Bool
sellEvaluation td chart rate =
  Ftd.side td == Ftd.None
  --Ftd.side td == Ftd.None || Ftd.side td == Ftd.Buy
  --Ftd.side td == Ftd.None || (Ftd.side td == Ftd.Buy  && 0 < chart - rate )

getQuantityLearning :: Ftd.FxTradeData -> Double -> Double
getQuantityLearning td chart = if ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25 < (Ftd.realizedPL td) / Gsd.quantityRate Gsd.gsd
                               then ((fromIntegral $ Gsd.maxUnit Gsd.gsd) * chart) / 25
                               else Ftd.realizedPL td / Gsd.quantityRate Gsd.gsd

getQuantityBacktest = getQuantityLearning

{-
getQuantityBacktest :: Ftd.FxTradeData -> Double -> Double
getQuantityBacktest _ _ = (Gsd.initalProperty Gsd.gsd) / (Gsd.quantityRate Gsd.gsd)

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


