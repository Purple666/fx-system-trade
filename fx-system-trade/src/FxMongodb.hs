{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module FxMongodb
  ( getChartListBack
  , getChartListForward
  , getOneChart
  , getStartChartFromDB
  , getEndChartFromDB
  , setFxTradeData
  , updateFxTradeData
  , writeFxSettingData
  , readFxSettingData
  , checkFxSettingData
  ) where


import           Control.Monad.Trans.Reader
import           Database.MongoDB
--import Debug.Trace
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified FxChartData                as Fcd
import qualified FxSetting                  as Fs
import qualified FxSettingData              as Fsd
import qualified FxTradeData                as Ftd
import qualified GlobalSettingData          as Gsd


getChartListBack :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListBack s l rl = do
  r <- getChartList (s - l) s
  minc <- getOneChart getStartChartFromDB
  if rl + length r < l && Fcd.no minc < s
    then do r' <- (++) <$> getChartListBack (s - l) l (rl + length r) <*> pure r
            if 0 < length r' - l
              then return $ drop (length r' - l) r'
              else return r'
    else return r

getChartListForward :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListForward s l rl = do
  r <- getChartList s (s + l)
  maxc <- getOneChart getEndChartFromDB
  if rl + length r < l && s < Fcd.no maxc
    then do r' <- (++) <$> pure r <*> getChartListForward (s + l) l (rl + length r)
            return $ take l r'
    else return r

getOneChart :: ReaderT MongoContext IO [Document] -> IO Fcd.FxChartData
getOneChart f = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" f
  close pipe
  r' <- mapM (\x -> return $ Fcd.FxChartData { Fcd.no    = typed $ valueAt "no"    x
                                             , Fcd.date  = typed $ valueAt "time"  x
                                             , Fcd.open  = typed $ valueAt "open"  x
                                             , Fcd.high  = typed $ valueAt "high"  x
                                             , Fcd.low   = typed $ valueAt "low"   x
                                             , Fcd.close = typed $ valueAt "close" x
                                             }
             ) r
  return $ head r'

getChartList :: Int -> Int -> IO [Fcd.FxChartData]
getChartList s e = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getChartListFromDB  s e
  close pipe
  mapM (\x -> return $ Fcd.FxChartData { Fcd.no    = typed $ valueAt "no"    x
                                       , Fcd.date  = typed $ valueAt "time"  x
                                       , Fcd.open  = typed $ valueAt "open"  x
                                       , Fcd.high  = typed $ valueAt "high"  x
                                       , Fcd.low   = typed $ valueAt "low"   x
                                       , Fcd.close = typed $ valueAt "close" x
                                       }
       ) r

getChartListFromDB :: Int -> Int -> ReaderT MongoContext IO [Document]
getChartListFromDB s e =
  rest =<< find (select ["no" =: ["$gte" =: s, "$lte" =: e]] "rate")

getStartChartFromDB :: ReaderT MongoContext IO [Document]
getStartChartFromDB =
  rest =<< find (select [] "rate") {sort = ["no" =: (1 :: Int)], limit = 1}

getEndChartFromDB :: ReaderT MongoContext IO [Document]
getEndChartFromDB =
  rest =<< find (select [] "rate") {sort = ["no" =: (-1 :: Int)], limit = 1}

setFxTradeData :: String-> Ftd.FxTradeData -> IO ()
setFxTradeData coName td = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setFxTradeDataToDB (T.pack coName) td
  close pipe

updateFxTradeData :: String -> Ftd.FxTradeData -> IO Ftd.FxTradeData
updateFxTradeData coName td = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB (T.pack coName)
  close pipe
  if null r
    then return td
    else head <$> mapM (\x -> return $ td { Ftd.chart      = read . typed $ valueAt "chart" x
                                          , Ftd.trSuccess  = typed $ valueAt "tr_success" x
                                          , Ftd.trFail     = typed $ valueAt "tr_fail" x
                                          , Ftd.profit     = typed $ valueAt "profit" x
                                          , Ftd.realizedPL = typed $ valueAt "realizedPL" x
                                          }) r

readFxSettingData :: Fsd.FxSettingData -> IO Fsd.FxSettingData
readFxSettingData  fsd = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB "fsd"
  close pipe
  if null r
    then return fsd
    else do fls <- head <$> mapM (\x -> return (read . typed $ valueAt "fls" x)) r
            fsl <- head <$> mapM (\x -> return (read . typed $ valueAt "fsl" x)) r
            return $ Fs.setFxSettingData fsd fls fsl

checkFxSettingData :: IO Bool
checkFxSettingData = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB "fsd"
  close pipe
  if null r
    then return True
    else return False

writeFxSettingData :: Fsd.FxSettingData -> IO Fsd.FxSettingData
writeFxSettingData fsd = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setFxSettingToDB (Fsd.learningSetting $ Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd)
  close pipe
  return fsd

getDataFromDB :: T.Text -> ReaderT MongoContext IO [Document]
getDataFromDB coName =
  rest =<< find (select [] coName)

setFxTradeDataToDB :: T.Text -> Ftd.FxTradeData -> Action IO ()
setFxTradeDataToDB coName td =
  upsert (select [] coName) [ "chart"       =: show (Ftd.chart td)
                          , "tr_success"  =: Ftd.trSuccess td
                          , "tr_fail"     =: Ftd.trFail td
                          , "profit"      =: Ftd.profit td
                          , "realizedPL"  =: Ftd.realizedPL td
                          ]

setFxSettingToDB :: Fsd.FxLearningSetting -> M.Map Fsd.FxSetting (Double, Int) -> Action IO ()
setFxSettingToDB fls fsl =
  upsert (select [] "fsd") [ "fls"  =: show fls
                         , "fsl"  =: show fsl
                         ]
