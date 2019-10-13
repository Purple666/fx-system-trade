{-# LANGUAGE OverloadedStrings #-}

module FxRedis
  ( getChartList
  , getEndChart
  , getOneChart
  ) where

import qualified FxChartData               as Fcd
import           Database.Redis
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Maybe
import           Data.Either

getChartList :: Int -> Int -> IO [Fcd.FxChartData]
getChartList s l = do
  let e = s + l - 1
  conn <- connect defaultConnectInfo { connectPort = UnixSocket "/var/tmp/redis/redis.sock" 
                                     }
  r <- runRedis conn $ zrange "fx" (fromIntegral s) (fromIntegral e)
  disconnect conn
  return . map (\x -> Fcd.FxChartData { Fcd.no    = fromIntegral . fromJust $ x ^? key "no" . _Integer
                                      , Fcd.date  = fromIntegral . fromJust $ x ^? key "time" . _Integer
                                      , Fcd.close = fromJust $ x ^? key "close" . _Double
                                      } ) $ fromRight [] r

getOneChart :: Int -> IO Fcd.FxChartData
getOneChart n = do
  head <$> getChartList n 1
  
getEndChart :: IO Fcd.FxChartData
getEndChart = do
  conn <- connect defaultConnectInfo { connectPort = UnixSocket "/var/tmp/redis/redis.sock" 
                                     }
  e <- runRedis conn $ zcard "fx"
  disconnect conn
  let n = fromIntegral $ (fromRight 0 e) - 1
  head <$> getChartList n 1
