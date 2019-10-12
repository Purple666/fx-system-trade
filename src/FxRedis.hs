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
  conn <- connect defaultConnectInfo { connectHost = "openshift.flg.jp"
                                     , connectPort = PortNumber 30379
                                     }
  r <- runRedis conn $ lrange "fx" (fromIntegral s) (fromIntegral e)
  return . map (\x -> Fcd.FxChartData { Fcd.no    = fromIntegral . fromJust $ x ^? key "no" . _Integer
                                      , Fcd.date  = fromIntegral . fromJust $ x ^? key "time" . _Integer
                                      , Fcd.close = fromJust $ x ^? key "close" . _Double
                                      } ) $ fromRight [] r

getOneChart :: Int -> IO Fcd.FxChartData
getOneChart n = do
  head <$> getChartList n 1
  
getEndChart :: IO Fcd.FxChartData
getEndChart = do
  conn <- connect defaultConnectInfo { connectHost = "openshift.flg.jp"
                                     , connectPort = PortNumber 30379
                                     }
  e <- runRedis conn $ llen "fx"
  let n = (fromRight 0 e) - 1
  r <- runRedis conn $ lindex "fx" n
  let x = fromJust $ fromRight (Just "") r
  return $ Fcd.FxChartData { Fcd.no    = fromIntegral . fromJust $ x ^? key "no" . _Integer
                           , Fcd.date  = fromIntegral . fromJust $ x ^? key "time" . _Integer
                           , Fcd.close = fromJust $ x ^? key "close" . _Double
                           }


