module FxTradeData
  ( FxTradeData (..)
  , FxTradeAlgorithmListCount (..)
  , FxSide (..)
  , FxEnvironment (..)
  , initFxTradeDataCommon
  , initFxTradeAlgorithmListCount
  , getWinRate
  , getEvaluationValue
  , getEvaluationValueList
  ) where

--import Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd

data FxTradeAlgorithmListCount =
  FxTradeAlgorithmListCount { alcOpen            :: Fad.FxalgorithmListCount
                            , alcCloseProfit     :: Fad.FxalgorithmListCount
                            , alcCloseLoss       :: Fad.FxalgorithmListCount
                            , alcTradeDate       :: Int
                            , alcTrade           :: Int
                            , alcFailProfit      :: Double
                            , alcSuccessProfit   :: Double
                            , alcSuccess         :: Int
                            , alcFail            :: Int
                            }

data FxTradeData =
  FxTradeData { chart              :: Fcd.FxChartData
              , tradeRate          :: Fcd.FxChartData
              , side               :: FxSide
              , trSuccess          :: Int
              , trFail             :: Int
              , profit             :: Double
              , unrealizedPL       :: Double
              , realizedPL         :: Double
              , environment        :: FxEnvironment
              , bearer             :: String
              , url                :: String
              } deriving (Show, Read)

data FxSide = None | Buy | Sell | Close deriving (Show, Read, Eq)

data FxEnvironment = Backtest | Practice | Production deriving (Show, Read)

instance Num FxTradeData where
  x - y = x { trSuccess          = trSuccess          x - trSuccess          y
            , trFail             = trFail             x - trFail             y
            , profit             = profit             x - profit             y
            , realizedPL         = realizedPL         x - realizedPL         y
            , unrealizedPL       = unrealizedPL       x - unrealizedPL       y
            }
  x + y = x { trSuccess          = trSuccess          x + trSuccess          y
            , trFail             = trFail             x + trFail             y
            , profit             = profit             x + profit             y
            , realizedPL         = realizedPL         x + realizedPL         y
            , unrealizedPL       = unrealizedPL       x + unrealizedPL       y
            }

  fromInteger _ = initFxTradeDataCommon { realizedPL       = 0
                                        , unrealizedPL     = 0
                                        }

instance Eq FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT


initFxTradeAlgorithmListCount :: FxTradeAlgorithmListCount
initFxTradeAlgorithmListCount =
  FxTradeAlgorithmListCount{ alcOpen             = Fad.zeroFxalgorithmListCount
                           , alcCloseProfit      = Fad.zeroFxalgorithmListCount
                           , alcCloseLoss        = Fad.zeroFxalgorithmListCount
                           , alcTradeDate        = 0
                           , alcTrade            = 0 
                           , alcFailProfit       = 0
                           , alcSuccessProfit    = 0
                           , alcSuccess          = 0 
                           , alcFail             = 0
                           }

initFxTradeDataCommon :: FxTradeData
initFxTradeDataCommon =
  FxTradeData { chart               = Fcd.initFxChartData
              , tradeRate           = Fcd.initFxChartData
              , side                = None
              , profit              = 0
              , trSuccess           = 0
              , trFail              = 0
              , realizedPL          = Gsd.initalProperty Gsd.gsd
              , unrealizedPL        = Gsd.initalProperty Gsd.gsd
              , environment         = Backtest
              , bearer              = ""
              , url                 = ""
              }

getEvaluationValue :: FxTradeData -> Double
getEvaluationValue x =
  (abs $ profit x) * ((unrealizedPL x / Gsd.initalProperty Gsd.gsd) - 1) * (getWinRatePure x) -- / (fromIntegral $ trSuccess x)
{-  
  if unrealizedPL x < Gsd.initalProperty Gsd.gsd && profit x < 0
  then - profit x * ((unrealizedPL x / Gsd.initalProperty Gsd.gsd) - 1) * (getWinRatePure x) ^ (4 :: Int) / ((fromIntegral $ trFail x) + (fromIntegral $ trSuccess x))
  else   
profit x
-}

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

