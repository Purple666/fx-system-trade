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
  , readResult
  , writeResult
  ) where

import           Control.Monad.Trans.Reader
import           Database.MongoDB
import           Control.Exception.Extra
import Debug.Trace
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified FxChartData                as Fcd
import qualified FxSetting                  as Fs
import qualified FxSettingData              as Fsd
import qualified FxTradeData                as Ftd
import qualified GlobalSettingData          as Gsd

getChartListBack :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListBack s l rl = do
  traceShow(s, l, rl) $ return ()
  r <- getChartList (s - l) s
  traceShow(length r) $ return ()
  minc <- getOneChart getStartChartFromDB
  r' <- if rl + length r < l && Fcd.no minc < s
        then (++) <$> getChartListBack (s - l) l (rl + length r) <*> pure r
        else return r
  if 0 < length r' - l
    then return $ drop (length r' - l) r'
    else return r'


getChartListForward :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListForward s l rl = do
  traceShow(s, l, rl) $ return ()
  r <- getChartList s (s + l)
  maxc <- getOneChart getEndChartFromDB
  r' <- if rl + length r < l && s < Fcd.no maxc
        then (++) <$> pure r <*> getChartListForward (s + l) l (rl + length r)
        else return r
  return $ take l r'

getOneChart :: ReaderT MongoContext IO [Document] -> IO Fcd.FxChartData
getOneChart f = do
  pipe <- retry 100 $ connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- retry 100 $ access pipe master "fx" f
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
  pipe <- retry 100 $ connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- retry 100 . access pipe master "fx" $ getChartListFromDB  s e
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
    else head <$> mapM (\x -> return $ td { Ftd.chart         = read . typed $ valueAt "chart"         x
                                          , Ftd.tradeRate     = read . typed $ valueAt "tradeRate"     x
                                          , Ftd.trSuccess     = read . typed $ valueAt "trSuccess"     x
                                          , Ftd.trFail        = read . typed $ valueAt "trFail"        x
                                          , Ftd.profit        = read . typed $ valueAt "profit"        x}) r

readFxSettingData :: String -> IO Fsd.FxSettingData
readFxSettingData coName = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB (T.pack $ "fsd_" ++ coName)
  close pipe
  if null r
    then return $ Fsd.initFxSettingData
    else do fs <- head <$> mapM (\x -> return (read . typed $ valueAt "fs" x)) r
            fsl <- head <$> mapM (\x -> return (read . typed $ valueAt "fsl" x)) r
            return $ Fs.setFxSettingData fs fsl

checkFxSettingData :: String -> IO Bool
checkFxSettingData coName = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB (T.pack $ "fsd_" ++ coName)
  close pipe
  if null r
    then return True
    else return False

writeFxSettingData :: String -> Fsd.FxSettingData -> IO (Fsd.FxSettingData)
writeFxSettingData coName fsd = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setFxSettingToDB (T.pack $ "fsd_" ++ coName) (Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd)
  close pipe
  return fsd

readResult :: String -> IO (Int, Int)
readResult coName = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB (T.pack $ "result_" ++ coName)
  close pipe
  if null r
    then return (0, 0)
    else do s <- head <$> mapM (\x -> return (read . typed $ valueAt "success" x)) r
            f <- head <$> mapM (\x -> return (read . typed $ valueAt "fail"    x)) r
            return (s , f)

writeResult :: String -> Int -> Int -> IO ()
writeResult coName s f = do
  pipe <- connect (readHostPort $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setResultToDB (T.pack $ "result_" ++ coName) s f 
  close pipe
  return ()

getDataFromDB :: T.Text -> ReaderT MongoContext IO [Document]
getDataFromDB coName =
  rest =<< find (select [] coName)

setFxTradeDataToDB :: T.Text -> Ftd.FxTradeData -> Action IO ()
setFxTradeDataToDB coName td =
  upsert (select [] coName) [ "chart"          =: (show $ Ftd.chart         td)    
                            , "tradeRate"      =: (show $ Ftd.tradeRate     td)    
                            , "trSuccess"      =: (show $ Ftd.trSuccess     td)       
                            , "trFail"         =: (show $ Ftd.trFail        td)       
                            , "profit"         =: (show $ Ftd.profit        td)       
                            ]

setFxSettingToDB :: T.Text -> Fsd.FxSetting -> M.Map Fsd.FxSetting (Double, Int) -> Action IO ()
setFxSettingToDB coName fs fsl =
  upsert (select [] coName ) [ "fs"  =: show fs
                             , "fsl" =: show fsl
                             ]

setResultToDB :: T.Text -> Int -> Int -> Action IO ()
setResultToDB coName s f = 
  upsert (select [] coName ) [ "success" =: show s
                             , "fail"    =: show f
                             ]
  

