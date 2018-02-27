{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module FxChartData
  ( FxChartData (..)
  , initFxChartData
  , getDate
  , getYear
  ) where

import Data.UnixTime
import qualified Data.ByteString.Char8 as LC (unpack, pack)

data FxChartData = FxChartData
  { date  :: Int
  , close :: Double }
  deriving (Show)

initFxChartData :: FxChartData
initFxChartData = 
  FxChartData { date  = 0
              , close = 0
              }
  
instance Num FxChartData where
  a + b = FxChartData { close = close a + close b
                      , date = 0 }
  a - b = FxChartData { close = close a - close b
                      , date = 0 }
  a * b = FxChartData { close = close a * close b
                      , date = 0 }
  fromInteger a = FxChartData { close = fromIntegral a    
                              , date = 0 }

instance Fractional FxChartData where
  a / b = FxChartData { close = close a / close b
                      , date = 0 }

instance Eq FxChartData where
  a == b = close a == close b
  
instance Ord FxChartData where
  compare a b
    | close a == close b  = EQ
    | close a <= close b  = LT
    | otherwise           = GT

getDate :: Int -> String
getDate n = LC.unpack . formatUnixTimeGMT (LC.pack "%Y/%m/%d") $ UnixTime (fromInteger ((fromIntegral n) * 60)) 0

getYear :: FxChartData -> Int
getYear c = read . LC.unpack . formatUnixTimeGMT (LC.pack "%Y%m%d") $ UnixTime (fromInteger ((fromIntegral $ date c) * 60)) 0

