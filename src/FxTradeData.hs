module FxTradeData
  ( FxTradeData (..)
  , FxSide (..)
  , FxEnvironment (..)
  , initFxTradeDataCommon
  , getWinRate
  , getWinRatePure
  ) where

import qualified Data.Map                as M
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd

data FxTradeData =
  FxTradeData { chart            :: Fcd.FxChartData
              , tradeRate        :: Fcd.FxChartData
              , unit             :: Int
              , side             :: FxSide
              , trSuccess        :: !Int
              , trFail           :: !Int
              , profit           :: !Double
              , realizedPL       :: !Double
              , chartLength      :: Int
              , maxUnit          :: Int
              , coName           :: String
              , environment      :: FxEnvironment
              , bearer           :: String
              , url              :: String
              } deriving (Show, Read)

data FxSide = None | Buy | Sell | Close deriving (Show, Read, Eq)

data FxEnvironment = Backtest | Practice | Production deriving (Show, Read, Eq)

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

getWinRatePure :: FxTradeData -> Double
getWinRatePure x =
  if trSuccess x + trFail x == 0
  then 0
  else fromIntegral (trSuccess x) / (fromIntegral (trSuccess x) + fromIntegral (trFail x))

getWinRate :: FxTradeData -> Double
getWinRate x = 100 * getWinRatePure x

