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
  let (_, xs')      = break (\x -> date x `mod` c == 0) xs
      (chart, xs'') = break (\x -> date x `mod` c == 0) $ tail xs'
      chart' = head xs' : chart
      fcd = (head chart') { open  = open    $ head chart'
                          , high  = maximum $ map high chart'
                          , low   = minimum $ map low  chart'
                          , close = close   $ last chart'
                          }
  in if null xs' || null chart' 
     then []
     else fcd : makeSimChart c xs''

{-
makeSimChart :: Int -> [FxChartData] -> [FxChartData]
makeSimChart c xs =
  filter (\x -> date x `mod` c == 0) xs
-}

getDate :: Int -> String
getDate n = LC.unpack . formatUnixTimeGMT (LC.pack "%Y/%m/%d") $ UnixTime (fromInteger ((fromIntegral n) * 60)) 0

getYear :: FxChartData -> Int
getYear c = read . LC.unpack . formatUnixTimeGMT (LC.pack "%Y%m%d") $ UnixTime (fromInteger ((fromIntegral $ date c) * 60)) 0

