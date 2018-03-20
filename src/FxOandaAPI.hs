{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FxOandaAPI
  ( close
  , open
  , updateFxTradeData
  ) where 

import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd
import qualified FxMongodb                as Fm
import qualified FxChartData              as Fcd
--import Debug.Trace
import Control.Exception.Extra
import GHC.Generics (Generic)
import Network.Wreq
import Control.Lens
import Data.Aeson
import Text.Printf
import Data.Time
import qualified Data.ByteString.Char8 as B

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
  { cid           :: Int
  , cunits        :: Int
  , cside         :: String
  }  deriving (Show, Generic)

data TradeReduced = TradeReduced
  { rid           :: Maybe Int
  , runits        :: Maybe Int
  , rside         :: Maybe String
  }  deriving (Show, Generic)

data OrdersBody = OrdersBody
  { instrument   :: String
  , time         :: String
  , price        :: Double
  , tradeOpened  :: TradeOpened
  , tradesClosed :: [TradesClosed]
  , tradeReduced :: TradeReduced
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

instance FromJSON TradeOpened where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON TradesClosed where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON TradeReduced where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON OrdersBody

instance FromJSON Positions where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON PositionsBody

instance FromJSON AccountsBody

close :: Ftd.FxTradeData -> IO Ftd.FxTradeData
close td = do
  (s, u, _) <- getOandaPosition td
  printf" %s : " . show =<< getZonedTime
  printf "Close - %d\n" u
  if s == Ftd.Buy
    then setOandaOrders td "sell" u
    else if s == Ftd.Sell
         then setOandaOrders td "buy" u
         else return ()
  updateFxTradeData td

open :: Ftd.FxTradeData -> Ftd.FxSide -> IO (Int, Ftd.FxTradeData)
open td side = do
  (b, _) <- getOandaBalance td
  p <- Fm.getOneChart  Fm.getEndChartFromDB 
  let u = truncate $ ((b / Gsd.quantityRate Gsd.gsd) * 25) / (Fcd.close p)
      u' = if Gsd.maxUnit Gsd.gsd < u
           then Gsd.maxUnit Gsd.gsd
           else u
  printf" %s : " . show =<< getZonedTime
  printf "Open - %s %f %d\n" (show side) b u'
  if side == Ftd.Buy
    then setOandaOrders td "buy" u'
    else if side == Ftd.Sell
         then setOandaOrders td "sell" u'
         else return ()
  td' <- updateFxTradeData td
  return (u', td')

updateFxTradeData :: Ftd.FxTradeData -> IO Ftd.FxTradeData
updateFxTradeData td = do
  (s, _, r) <- getOandaPosition td
  (b, upl) <- getOandaBalance td
  c <- Fm.getOneChart Fm.getEndChartFromDB 
  return $ td { Ftd.rate         = (Ftd.rate td) { Fcd.date  = Fcd.date c
                                                 , Fcd.close = r
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

setOandaOrders :: Ftd.FxTradeData -> String -> Int -> IO ()
setOandaOrders td s u = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  _ <- retry 100 $ postWith opts (Ftd.url td ++ "/orders")
       ["instrument" := ("USD_JPY" :: String), "units" := u, "side" := s, "type" := ("market" :: String)]
       >>= asJSON :: IO (Response OrdersBody)
  return ()

getOandaPosition :: Ftd.FxTradeData -> IO (Ftd.FxSide, Int, Double)
getOandaPosition td = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ getWith opts (Ftd.url td ++ "/positions")
       >>= asJSON
  let ps = positions $ r ^. responseBody
      s = if ps == []
          then Ftd.None
          else let st = pside $ head ps
               in if st == "buy"
                  then Ftd.Buy
                  else if st == "sell"
                       then Ftd.Sell
                       else Ftd.None
      u = if ps == []
          then 0
          else punits $ head ps
      p = if ps == []
          then 0
          else pavgPrice $ head ps
  return (s, u, p)

