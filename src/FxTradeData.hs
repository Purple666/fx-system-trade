
module FxTradeData
  ( FxTradeData (..)
  , FxSide (..)
  , FxEnvironment (..)
  , resetFxalgorithmListCount
  , initFxTradeData
  , getWinRate
  , evaluationOk
  , getEvaluationValueList
  , getEvaluationValue
  ) where

--import Debug.Trace
import qualified GlobalSettingData        as Gsd
import qualified FxChartData              as Fcd
import qualified FxTechnicalAnalysisData  as Fad

data FxTradeData =
  FxTradeData { chart              :: Fcd.FxChartData
              , rate               :: Fcd.FxChartData
              , alcOpen            :: Fad.FxalgorithmListCount
              , alcCloseProfit     :: Fad.FxalgorithmListCount
              , alcCloseLoss       :: Fad.FxalgorithmListCount
              , side               :: FxSide
              , trSuccessDate      :: Int
              , trSuccess          :: Int
              , trFail             :: Int
              , profit             :: Double
              , unrealizedPL       :: Double
              , realizedPL         :: Double
              , environment        :: FxEnvironment
              , bearer             :: String
              , url                :: String
              } deriving (Show)

data FxSide = None | Buy | Sell | Close deriving (Show, Eq)

data FxEnvironment = Backtest | Practice | Production deriving (Show, Eq)

instance Eq FxTradeData where
  x == y = getEvaluationValue x == getEvaluationValue y

instance Ord FxTradeData where
  compare x y
    | getEvaluationValue x == getEvaluationValue y    =  EQ
    | getEvaluationValue x <= getEvaluationValue y    =  LT
    | otherwise                                       =  GT

instance Num FxTradeData where
  x - y = x { trSuccess    = trSuccess    x - trSuccess    y
            , trFail       = trFail       x - trFail       y
            , profit       = profit       x - profit       y
            , realizedPL   = realizedPL   x - realizedPL   y
            , unrealizedPL = unrealizedPL x - unrealizedPL y
            }
  x + y = x { trSuccess    = trSuccess    x + trSuccess    y
            , trFail       = trFail       x + trFail       y
            , profit       = profit       x + profit       y
            , realizedPL   = realizedPL   x + realizedPL   y
            , unrealizedPL = unrealizedPL x + unrealizedPL y
            }

  fromInteger _ = initFxTradeDataCommon { realizedPL       = 0
                                        , unrealizedPL     = 0
                                        }

resetFxalgorithmListCount :: FxTradeData -> FxTradeData
resetFxalgorithmListCount td =
  td { alcOpen        = Fad.zeroFxalgorithmListCount
     , alcCloseProfit = Fad.zeroFxalgorithmListCount
     , alcCloseLoss   = Fad.zeroFxalgorithmListCount
     }
  
initFxTradeDataCommon :: FxTradeData
initFxTradeDataCommon = 
  FxTradeData { chart            = 0
              , rate             = 0
              , side             = None
              , alcOpen          = Fad.zeroFxalgorithmListCount
              , alcCloseProfit   = Fad.zeroFxalgorithmListCount
              , alcCloseLoss     = Fad.zeroFxalgorithmListCount
              , trSuccessDate    = 0
              , trSuccess        = 0
              , trFail           = 0
              , profit           = 0
              , realizedPL       = Gsd.initalProperty Gsd.gsd
              , unrealizedPL     = Gsd.initalProperty Gsd.gsd
              , environment      = Backtest
              , bearer           = ""
              , url              = ""
              }

initFxTradeData :: FxEnvironment -> FxTradeData
initFxTradeData Backtest =
  initFxTradeDataCommon { environment      = Backtest
                        , bearer           = ""
                        , url              = ""
                        }
initFxTradeData Practice =
  initFxTradeDataCommon { environment      = Practice
                        , bearer           = Gsd.tradePracticeBearer Gsd.gsd
                        , url              = Gsd.tradePracticeUrl  Gsd.gsd
                        }
initFxTradeData Production =
  initFxTradeDataCommon { environment      = Production
                        , bearer           = Gsd.tradeProductionBearer Gsd.gsd
                        , url              = Gsd.tradeProductionUrl  Gsd.gsd
                        }
     
getWinRate :: FxTradeData -> Double
getWinRate x = 100 * getWinRatePure x

getWinRatePure :: FxTradeData -> Double
getWinRatePure x =
  if (fromIntegral $ trSuccess x) + (fromIntegral $ trFail x) == (0 :: Double)
  then 0
  else (fromIntegral $ trSuccess x) / ((fromIntegral $ trSuccess x) + (fromIntegral $ trFail x))

evaluationOk :: FxTradeData -> [FxTradeData] -> Bool
evaluationOk tdl tdlt =
  (and $ map (\x -> 0 < getEvaluationValue x) tdlt) && 0 < getEvaluationValueList tdlt && 0 < getEvaluationValue tdl 

getEvaluationValueList :: [FxTradeData] -> Double
getEvaluationValueList x =
  sum $ map (\y -> getEvaluationValue y) x

getEvaluationValue :: FxTradeData -> Double
getEvaluationValue x =
  profit x * getWinRatePure x ^ (2 :: Int)
 {-
  if profit x < 0 && realizedPL x < 0
  then - profit x * (realizedPL x / Gsd.initalProperty Gsd.gsd) * getWinRatePure x ^ 4
  else   profit x * (realizedPL x / Gsd.initalProperty Gsd.gsd) * getWinRatePure x ^ 4
  profit x * (realizedPL x / Gsd.initalProperty Gsd.gsd) * (logBase 10 . fromIntegral $ trSuccess x) * getWinRatePure x ^ 4
-}

