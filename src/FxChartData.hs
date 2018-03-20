module FxChartData
  ( FxChartData (..)
  , initFxChartData
  , makeSimChart
  , getDate
  , getYear
  ) where

import Data.UnixTime
import qualified Data.ByteString.Char8 as LC (unpack, pack)

data FxChartData = FxChartData
  { date  :: Int
  , open  :: Double
  , high  :: Double
  , low   :: Double
  , close :: Double
  }
  deriving (Show, Read, Eq)

initFxChartData :: FxChartData
initFxChartData = 
  FxChartData { date  = 0
              , open  = 0
              , high  = 0
              , low   = 0
              , close = 0
              }

makeSimChart :: Int -> [FxChartData] -> [FxChartData]
makeSimChart _ [] = []
makeSimChart c xs =
  let chart = take c xs
      fcd = (head xs) { open  = open    $ head chart 
                      , high  = maximum $ map high xs
                      , low   = minimum $ map low  xs
                      , close = close   $ last chart
                      }
  in fcd : (makeSimChart c $ drop c xs)

getDate :: Int -> String
getDate n = LC.unpack . formatUnixTimeGMT (LC.pack "%Y/%m/%d") $ UnixTime (fromInteger ((fromIntegral n) * 60)) 0

getYear :: FxChartData -> Int
getYear c = read . LC.unpack . formatUnixTimeGMT (LC.pack "%Y%m%d") $ UnixTime (fromInteger ((fromIntegral $ date c) * 60)) 0

