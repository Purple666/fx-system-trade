{-# LANGUAGE OverloadedStrings #-}

module FxTweet
  ( tweetClose
  , tweetOpen
  , tweetWeek
  ) where

import qualified FxChartData  as Fcd
import qualified FxTradeData  as Ftd
import           Network.Wreq
import           Text.Printf

tweet :: String -> IO ()
tweet t = do
  let opts = defaults &
             auth ?~ oauth1Auth "HjnDeOzLAz8akOvdWFwAs4jpk" "IKmctdySoondQMHyjEEyi3msIyrt9LebdKR6pSFPlOblWeZxgK" "94802117-TIe0khpHJgQyDjfeASKoqY54wFtJT7fylLHZ6gRdb" "DnARCXEzJk15X0rhcgHTwPzseY3s6kXga6nFrzOBXIPr7"
  _ <- postWith opts "https://api.twitter.com/1.1/statuses/update.json" ["status" := t]
  return ()

tweetClose :: Ftd.FxTradeData -> Ftd.FxTradeData -> IO ()
tweetClose td td' = do
  let fmts =  "#FxSystemTrade(" ++ show (Ftd.environment td') ++ ")\n" ++
              "[Close]\n" ++
              "Rate : %.2f -> %.2f\n" ++
              "Side : " ++ show (Ftd.side td) ++ "\n" ++
              "Realized PL : %.0f (%+.0f)\n" ++
              "Win Rate : %.2f %% (%d - %d)\n"
      t = printf fmts
          (Fcd.close $ Ftd.tradeRate td)
          (Fcd.close $ Ftd.chart td')
          (Ftd.realizedPL td')
          (Ftd.realizedPL $ td' - td)
          (Ftd.getWinRate td')
          (Ftd.trSuccess td')
          (Ftd.trFail td') :: String
  tweet t
  return ()

tweetOpen :: Ftd.FxTradeData -> Int -> IO ()
tweetOpen td units = do
  let fmts =  "#FxSystemTrade(" ++ show (Ftd.environment td) ++ ")\n" ++
              "[Open]\n" ++
              "Rate : %.2f\n" ++
              "Side : " ++ show (Ftd.side td) ++ "\n" ++
              "Unit : %d\n"
      t = printf fmts
          (Fcd.close $ Ftd.tradeRate td)
          units :: String
  tweet t
  return ()

tweetWeek :: Ftd.FxTradeData -> Ftd.FxTradeData -> IO ()
tweetWeek td td' = do
  nd  <- Fcd.getDate . Fcd.date $ Ftd.chart td
  nd' <- Fcd.getDate . Fcd.date $ Ftd.chart td'
  let fmts =  "#FxSystemTrade(%s)\n" ++
              "[%s - %s]\n" ++
              "Rate : %.2f -> %.2f\n" ++
              "Realized PL : %.0f (%+.0f)\n" ++
              "Win Rate : %.2f %% (%d - %d)\n"
      t = printf fmts
          (show $ Ftd.environment td')
          nd
          nd'
          (Fcd.close $ Ftd.chart td)
          (Fcd.close $ Ftd.chart td')
          (Ftd.realizedPL td')
          (Ftd.realizedPL $ td' - td)
          (Ftd.getWinRate td')
          (Ftd.trSuccess td')
          (Ftd.trFail td') :: String
  tweet t
  return ()
