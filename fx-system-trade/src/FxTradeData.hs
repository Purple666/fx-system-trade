module FxTradeData
  ( FxTradeData (..)
  , FxSide (..)
  , FxEnvironment (..)
  , initFxTradeDataCommon
  , getWinRate
  , getWinRatePure
  ) where

--import Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd

data FxTradeData =
  FxTradeData { chart           :: Fcd.FxChartData
              , rate            :: Fcd.FxChartData
              , alcOpen         :: Fad.FxalgorithmListCount
              , alcCloseProfit  :: Fad.FxalgorithmListCount
              , alcCloseLoss    :: Fad.FxalgorithmListCount
              , side            :: FxSide
              , trTradeDate     :: Int
              , trTrade         :: Int
              , failProfitCount :: Int
              , failProfit      :: Double
              , trSuccess       :: Int
              , trFail          :: Int
              , profit          :: Double
              , unrealizedPL    :: Double
              , realizedPL      :: Double
              , environment     :: FxEnvironment
              , bearer          :: String
              , url             :: String
              } deriving (Show, Read)

data FxSide = None | Buy | Sell | Close deriving (Show, Read, Eq)

data FxEnvironment = Backtest | Practice | Production deriving (Show, Read)

instance Num FxTradeData where
  x - y = x { trSuccess       = trSuccess       x - trSuccess       y
            , trFail          = trFail          x - trFail          y
            , failProfitCount = failProfitCount x - failProfitCount y
            , failProfit      = failProfit      x - failProfit      y
            , profit          = profit          x - profit          y
            , realizedPL      = realizedPL      x - realizedPL      y
            , unrealizedPL    = unrealizedPL    x - unrealizedPL    y
            }
  x + y = x { trSuccess       = trSuccess       x + trSuccess       y
            , trFail          = trFail          x + trFail          y
            , failProfitCount = failProfitCount x + failProfitCount y
            , failProfit      = failProfit      x + failProfit      y
            , profit          = profit          x + profit          y
            , realizedPL      = realizedPL      x + realizedPL      y
            , unrealizedPL    = unrealizedPL    x + unrealizedPL    y
            }

  fromInteger _ = initFxTradeDataCommon { realizedPL       = 0
                                        , unrealizedPL     = 0
                                        }

getWinRate :: FxTradeData -> Double
getWinRate x = 100 * getWinRatePure x

getWinRatePure :: FxTradeData -> Double
getWinRatePure x =
  if fromIntegral (trSuccess x) + fromIntegral (trFail x) == (0 :: Double)
  then 0
  else fromIntegral (trSuccess x) / (fromIntegral (trSuccess x) + fromIntegral (trFail x))

initFxTradeDataCommon :: FxTradeData
initFxTradeDataCommon =
  FxTradeData { chart            = Fcd.initFxChartData
              , rate             = Fcd.initFxChartData
              , side             = None
              , alcOpen          = Fad.zeroFxalgorithmListCount
              , alcCloseProfit   = Fad.zeroFxalgorithmListCount
              , alcCloseLoss     = Fad.zeroFxalgorithmListCount
              , trTradeDate      = 0
              , trTrade          = 0
              , trSuccess        = 0
              , trFail           = 0
              , failProfitCount  = 0
              , failProfit       = 0
              , profit           = 0
              , realizedPL       = Gsd.initalProperty Gsd.gsd
              , unrealizedPL     = Gsd.initalProperty Gsd.gsd
              , environment      = Backtest
              , bearer           = ""
              , url              = ""
              }


