module FxChartData
  ( FxChartData (..)
  , initFxChartData
  , getDate
  , getYear
  ) where

import qualified Data.ByteString.Char8 as LC (pack, unpack)
import           Data.UnixTime
import           Control.DeepSeq

data FxChartData = FxChartData
  { no    :: Int
  , date  :: Int
  , open  :: Double
  , high  :: Double
  , low   :: Double
  , close :: Double
  }
  deriving (Show, Read, Eq, NFData)

initFxChartData :: FxChartData
initFxChartData =
  FxChartData { no    = 0
              , date  = 0
              , open  = 0
              , high  = 0
              , low   = 0
              , close = 0
              }

getDate :: Int -> IO String
getDate n = LC.unpack <$> formatUnixTime (LC.pack "%Y/%m/%d %H:%M") (UnixTime (fromInteger (fromIntegral n * 60)) 0)

getYear :: FxChartData -> Int
getYear c = read . LC.unpack . formatUnixTimeGMT (LC.pack "%Y%m%d") $ UnixTime (fromInteger (fromIntegral (date c) * 60)) 0

