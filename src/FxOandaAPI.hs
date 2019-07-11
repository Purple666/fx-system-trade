{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module FxOandaAPI
  ( close
  , open
  , updateFxTradeData
  , getNowPrices
  ) where

import qualified FxChartData             as Fcd
import qualified FxMongodb               as Fm
import qualified FxTime                  as Ftm
import qualified FxTradeData             as Ftd
import qualified GlobalSettingData       as Gsd
import Debug.Trace
import           Control.Exception.Extra
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8   as B
import           GHC.Generics            (Generic)
import           Network.Wreq
import           Text.Printf

data Positions = Positions
  { pinstrument :: String
  , punits      :: Int
  , pside       :: String
  , pavgPrice   :: Double
  } deriving (Show, Generic, Eq)

data PositionsBody = PositionsBody
  { positions    :: [Positions]
  } deriving (Show, Generic, Eq)


data TradeOpened = TradeOpened
  { oid           :: Maybe Int
  , ounits        :: Maybe Int
  , oside         :: Maybe String
  , otakeProfit   :: Maybe Int
  , ostopLoss     :: Maybe Int
  , otrailingStop :: Maybe Int
  }  deriving (Show, Generic)

data TradesClosed = TradeClosed
  { cid    :: Int
  , cunits :: Int
  , cside  :: String
  }  deriving (Show, Generic)

data TradeReduced = TradeReduced
  { rid    :: Maybe Int
  , runits :: Maybe Int
  , rside  :: Maybe String
  }  deriving (Show, Generic)

data OrdersBody = OrdersBody
  { instrument   :: String
  , time         :: String
  , price        :: Double
  , tradeOpened  :: TradeOpened
  , tradesClosed :: [TradesClosed]
  , tradeReduced :: TradeReduced
  } deriving (Show, Generic)

data Prices = Prices
  { ninstrument :: String
  , ntime       :: String
  , nbid        :: Double
  , nask        :: Double
  } deriving (Show, Generic)

data PricesBody = PricesBody
  { prices :: [Prices]
  } deriving (Show, Generic)

data AccountsBody = AccountsBody
  { accountId       :: Int
  , accountName     :: String
  , balance         :: Double
  , unrealizedPl    :: Double
  , realizedPl      :: Double
  , marginUsed      :: Double
  , marginAvail     :: Double
  , openTrades      :: Int
  , openOrders      :: Int
  , marginRate      :: Double
  , accountCurrency :: String
  } deriving (Show, Generic)

instance FromJSON OrdersBody

instance FromJSON TradeOpened where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON TradesClosed where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON TradeReduced where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON PositionsBody

instance FromJSON Positions where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON PricesBody

instance FromJSON Prices where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON AccountsBody

getNowPrices :: Ftd.FxTradeData -> IO Fcd.FxChartData
getNowPrices td = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td] &
             param "instruments" .~ ["USD_JPY"]
  r <- retry 100 $ getWith opts "https://api-fxpractice.oanda.com/v1/prices"
       >>= asJSON
  e <- Fm.getOneChart Fm.getEndChartFromDB
  return $ e { Fcd.close = nbid . head . prices $ r ^. responseBody
             , Fcd.high  = nbid . head . prices $ r ^. responseBody
             , Fcd.low  = nbid . head . prices $ r ^. responseBody
             }

close :: Ftd.FxTradeData -> IO Ftd.FxTradeData
close td = do
  (s, u, _) <- getOandaPosition td
  printf "%s : " =<< Ftm.getLogTime
  printf "Close - %d\n" u
  updateFxTradeData =<< if s == Ftd.Buy
                        then setOandaOrders td "sell" u
                        else if s == Ftd.Sell
                             then setOandaOrders td "buy" u
                             else return td

open :: Ftd.FxTradeData -> Ftd.FxSide -> IO (Int, Ftd.FxTradeData)
open td side = do
  (b, _) <- getOandaBalance td
  p <- getNowPrices td
  let u = truncate $ ((b / Gsd.quantityRate Gsd.gsd) * 25) / Fcd.close p
      u' = if Gsd.maxUnit Gsd.gsd < u
           then Gsd.maxUnit Gsd.gsd
           else u
  printf "%s : " =<< Ftm.getLogTime
  printf "Open - %s %f %d %3.6f\n" (show side) b u' (Fcd.close p)
  td'' <- updateFxTradeData =<< if side == Ftd.Buy
                                then setOandaOrders td "buy" u'
                                else if side == Ftd.Sell
                                     then setOandaOrders td "sell" u'
                                     else return td
  return (u', td'')

updateFxTradeData :: Ftd.FxTradeData -> IO Ftd.FxTradeData
updateFxTradeData td = do
  (s, _, r) <- getOandaPosition td
  (b, upl) <- getOandaBalance td
  return $ td { Ftd.tradeRate    = (Ftd.tradeRate td) { Fcd.close = r
                                                      }
              , Ftd.side         = s
              , Ftd.realizedPL   = b
              , Ftd.unrealizedPL = b + upl
              }

getOandaBalance :: Ftd.FxTradeData -> IO (Double, Double)
getOandaBalance td = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ getWith opts (Ftd.url td)
       >>= asJSON
  let b =  balance $ r ^. responseBody
      upl = unrealizedPl $ r ^. responseBody
  return (b, upl)

setOandaOrders :: Ftd.FxTradeData -> String -> Int -> IO Ftd.FxTradeData
setOandaOrders td s u = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ postWith opts (Ftd.url td ++ "/orders")
       ["instrument" := ("USD_JPY" :: String), "units" := u, "side" := s, "type" := ("market" :: String)]
       >>= asJSON
  return $ td { Ftd.chart = (Ftd.chart td) { Fcd.close = price $ r ^. responseBody
                                           }
              }

getOandaPosition :: Ftd.FxTradeData -> IO (Ftd.FxSide, Int, Double)
getOandaPosition td = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ getWith opts (Ftd.url td ++ "/positions")
       >>= asJSON
  let ps = positions $ r ^. responseBody
      s = if null ps
          then Ftd.None
          else let st = pside $ head ps
               in if st == "buy"
                  then Ftd.Buy
                  else if st == "sell"
                       then Ftd.Sell
                       else Ftd.None
      u = if null ps
          then 0
          else punits $ head ps
      p = if null ps
          then 0
          else pavgPrice $ head ps
  return (s, u, p)

