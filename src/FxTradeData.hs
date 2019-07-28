module FxTradeData
  ( FxTradeData (..)
  , FxSide (..)
  , FxEnvironment (..)
  , initFxTradeDataCommon
  , getWinRate
  , getEvaluationValue
  , getEvaluationValueList
  ) where

import Debug.Trace
import qualified Data.Map          as M
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd

data FxTradeData =
  FxTradeData { chart              :: Fcd.FxChartData
              , tradeRate          :: Fcd.FxChartData
              , unit               :: Int
              , side               :: FxSide
              , trSuccess          :: Int
              , trFail             :: Int
              , profit             :: Double
              , realizedPL         :: Double
              , chartLength        :: Int
              , maxUnit            :: Int
              , coName             :: String
              , environment        :: FxEnvironment
              , bearer             :: String
              , url                :: String
              } deriving (Show, Read)

data FxSide = None | Buy | Sell | Close deriving (Show, Read, Eq)

data FxEnvironment = Backtest | Practice | Production deriving (Show, Read, Eq)

instance Num FxTradeData where
  x - y = x { unit       = unit       x - unit        y
            , trSuccess  = trSuccess  x - trSuccess   y
            , trFail     = trFail     x - trFail      y
            , profit     = profit     x - profit      y
            , realizedPL = realizedPL x - realizedPL  y
            }
  x + y = x { unit       = unit       x + unit        y
            , trSuccess  = trSuccess  x + trSuccess   y
            , trFail     = trFail     x + trFail      y
            , profit     = profit     x + profit      y
            , realizedPL = realizedPL x + realizedPL  y
            }

  fromInteger _ = initFxTradeDataCommon { realizedPL       = 0
                                        }

instance Eq FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT


initFxTradeDataCommon :: FxTradeData
initFxTradeDataCommon =
  FxTradeData { chart               = Fcd.initFxChartData
              , tradeRate           = Fcd.initFxChartData
              , unit                = 0
              , side                = None
              , profit              = 0
              , trSuccess           = 0
              , trFail              = 0
              , realizedPL          = Gsd.initalProperty Gsd.gsd
              , chartLength         = 0
              , maxUnit             = 0
              , coName              = ""
              , environment         = Backtest
              , bearer              = ""
              , url                 = ""
              }

getEvaluationValue :: FxTradeData -> Double
getEvaluationValue x =
  (profit x * realizedPL x) / (fromIntegral $ chartLength x * getWinRatePure x ^ 4)
--  
{-  
  profit x
  unrealizedPL x
  if profit x < 0 && unrealizedPL x < Gsd.initalProperty Gsd.gsd
  then - (profit x * (unrealizedPL x - Gsd.initalProperty Gsd.gsd) * getWinRatePure x ^ 4)
  else   (profit x * (unrealizedPL x - Gsd.initalProperty Gsd.gsd) * getWinRatePure x ^ 4)
  profit x
  ((fromIntegral $ trSuccess x) * profit x * unrealizedPL x) / (fromIntegral $ chartLength x)

unrealizedPL x / (fromIntegral $ chartLength x)
  
unrealizedPL x / (fromIntegral $ chartLength x)
  --(profit x * unrealizedPL x) / (fromIntegral $ chartLength x)
  (profit x * unrealizedPL x * getWinRatePure x ^ 4) / (fromIntegral $ chartLength x)
-}  
--  (profit x) / (fromIntegral $ chartLength x)
--  profit x / (fromIntegral $ chartLength x)

getEvaluationValueList :: [FxTradeData] -> Double
getEvaluationValueList tdlt =
  sum $ map getEvaluationValue tdlt

getWinRatePure :: FxTradeData -> Double
getWinRatePure x =
  if trSuccess x + trFail x == 0
  then 0
  else (fromIntegral $ trSuccess x) / ((fromIntegral $ trSuccess x) + (fromIntegral $ trFail x))

getWinRate :: FxTradeData -> Double
getWinRate x = 100 * getWinRatePure x

