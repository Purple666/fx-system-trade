{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module FxOandaAPI
  ( close
  , open
  , closeOpen
  , updateFxTradeData
  , getNowPrices
  ) where

import           Control.Exception.Extra
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8   as B
import           Data.Maybe
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxMongodb               as Fm
import qualified FxPrint                 as Fp
import qualified FxTime                  as Ftm
import qualified FxTradeData             as Ftd
import           GHC.Generics            (Generic)
import qualified GlobalSettingData       as Gsd
import           Network.Wreq
import           Text.Printf

data Pricing = Pricing
  { pi_prices     :: [Price]
  } deriving (Show, Generic)

data Price = Price
  { pr_bids :: [PriceBucket]
  , pr_asks :: [PriceBucket]
  } deriving (Show, Generic)

data PriceBucket = PriceBucket
  { pb_price :: String
  } deriving (Show, Generic)

instance FromJSON Pricing where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON Price where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON PriceBucket where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

data AccountBody = AccountBody
  { account :: Account
  } deriving (Show, Generic)

data Account = Account
  { balance      :: String
  , unrealizedPL :: String
  } deriving (Show, Generic)

instance FromJSON AccountBody
instance FromJSON Account

data PositionsBody = PositionsBody
  { positions    :: [Position]
  } deriving (Show, Generic)

data Position = Position
  { long  :: PositionSide
  , short :: PositionSide
  } deriving (Show, Generic)

data PositionSide = PositionSide
  { units        :: String
  , averagePrice :: Maybe String
  } deriving (Show, Generic)

instance FromJSON PositionsBody
instance FromJSON Position
instance FromJSON PositionSide

data Order = Order
  { order :: OrderRequest
  } deriving (Show, Generic)

data OrderRequest = OrderRequest
  { or_type         :: String
  , or_instrument   :: String
  , or_units        :: Int
  , or_timeInForce  :: String
  , or_positionFill :: String
  } deriving (Show, Generic)

instance ToJSON Order where
instance ToJSON OrderRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 }

getNowPrices :: Ftd.FxTradeData -> IO Fcd.FxChartData
getNowPrices td = do
  let opts = defaults &
             header "Authorization" .~ [B.pack $ Ftd.bearer td] &
             header "Content-Type" .~  ["application/json"] &
             param "instruments" .~ ["USD_JPY"]
  r <- retry 100 $ getWith opts (Ftd.url td ++ "/pricing")
       >>= asJSON
  e <- Fm.getOneChart Fm.getEndChartFromDB
  let ask = read . pb_price . head . pr_asks . head . pi_prices $ r ^. responseBody
      bid = read . pb_price . head . pr_bids . head . pi_prices $ r ^. responseBody
  return $ e { Fcd.close = if Ftd.side td == Ftd.Buy
                           then ask
                           else if Ftd.side td == Ftd.Sell
                                then bid
                                else (ask + bid) / 2
             }

closeOpen :: Ftd.FxTradeData -> Ftd.FxTradeData -> IO Ftd.FxTradeData
closeOpen tdo td = do
  (s, cu, _) <- getPosition td
  (b, _) <- getBalance td
  p <- getNowPrices td
  let ou = truncate $ ((b / Gsd.quantityRate Gsd.gsd) * 25) / Fcd.close p
      ou' = if Ftd.maxUnit td `div` 2 < ou
            then Ftd.maxUnit td `div` 2
            else ou
  (open, close) <- if s == Ftd.Buy
                   then do setOrders td (-(cu + ou'))
                           return (Ftd.Sell, Ftd.Buy)
                   else if s == Ftd.Sell
                        then do setOrders td (-cu + ou')
                                return (Ftd.Buy, Ftd.Sell)
                        else return (Ftd.None, Ftd.None)
  td' <- updateFxTradeData td
  Fm.setFxTradeData (Ftd.coName td') td'
  printf "%s : " =<< Ftm.getLogTime
  printf "closeOpen - %f %d %d %3.6f\n" b cu ou' (Fcd.close p)
  Fp.printTradeResult open close tdo td' ou'
  return td'

close :: Ftd.FxTradeData -> Ftd.FxTradeData -> IO Ftd.FxTradeData
close tdo td = do
  (s, u, _) <- getPosition td
  if s == Ftd.Buy || s == Ftd.Sell
    then setOrders td (-u)
    else return ()
  td' <- updateFxTradeData td
  Fm.setFxTradeData (Ftd.coName td') td'
  printf "%s : " =<< Ftm.getLogTime
  printf "Close - %d\n" u
  Fp.printTradeResult Ftd.None s tdo td' 0
  return td'

open :: Ftd.FxTradeData -> Ftd.FxTradeData -> Ftd.FxSide -> IO Ftd.FxTradeData
open tdo td side = do
  (b, _) <- getBalance td
  p <- getNowPrices td
  let u = truncate $ ((b / Gsd.quantityRate Gsd.gsd) * 25) / Fcd.close p
      u' = if Ftd.maxUnit td `div` 2 < u
           then Ftd.maxUnit td `div` 2
           else u
  if side == Ftd.Buy
    then setOrders td u'
    else if side == Ftd.Sell
         then setOrders td (-u')
         else return ()
  td' <- updateFxTradeData td
  Fm.setFxTradeData (Ftd.coName td') td'
  printf "%s : " =<< Ftm.getLogTime
  printf "Open - %s %f %d %3.6f\n" (show side) b u' (Fcd.close p)
  Fp.printTradeResult side Ftd.None tdo td' u'
  return td'

updateFxTradeData :: Ftd.FxTradeData -> IO Ftd.FxTradeData
updateFxTradeData td = do
  (s, _, r) <- getPosition td
  (b, upl) <- getBalance td
  return $ td { Ftd.tradeRate    = (Ftd.tradeRate td) { Fcd.close = r
                                                      }
              , Ftd.side         = s
              , Ftd.realizedPL   = b
              }

getBalance :: Ftd.FxTradeData -> IO (Double, Double)
getBalance td = do
  let opts = defaults &
             header "Content-Type" .~  ["application/json"] &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ getWith opts (Ftd.url td)
       >>= asJSON
  let b =  read . balance . account $ r ^. responseBody
      upl = read . unrealizedPL . account $ r ^. responseBody
  return (b, upl)

setOrders :: Ftd.FxTradeData -> Int -> IO ()
setOrders td u = do
  let opts = defaults &
             header "Content-Type" .~  ["application/json"] &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ postWith opts (Ftd.url td ++ "/orders") (toJSON Order { order = OrderRequest { or_type         = "MARKET"
                                                                                                , or_instrument   = "USD_JPY"
                                                                                                , or_units        = u
                                                                                                , or_timeInForce  = "FOK"
                                                                                                , or_positionFill = "DEFAULT"
                                                                                                }
                                                                         })
  -- traceShow(r) $ return ()
  return ()

getPosition :: Ftd.FxTradeData -> IO (Ftd.FxSide, Int, Double)
getPosition td = do
  let opts = defaults &
             header "Content-Type" .~  ["application/json"] &
             header "Authorization" .~ [B.pack $ Ftd.bearer td]
  r <- retry 100 $ getWith opts (Ftd.url td ++ "/openPositions")
       >>= asJSON
  let ps = positions $ r ^. responseBody
  return $ if null ps
           then (Ftd.None, 0, 0)
           else let lu = read . units . long  $ head ps
                    su = read . units . short $ head ps
                in if lu /= 0
                   then (Ftd.Buy, lu, read . fromJust . averagePrice . long  $ head ps)
                   else if su /= 0
                        then (Ftd.Sell, su, read . fromJust . averagePrice . short $ head ps)
                        else (Ftd.None, 0, 0)
