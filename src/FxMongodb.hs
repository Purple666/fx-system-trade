{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
  ) where


import Database.MongoDB
import Control.Monad.Trans.Reader
--import Debug.Trace
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxSetting                as Fs
import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd


getChartListBack :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListBack s l rl = do
  r <- getChartList (s - l) s
  minc <- getOneChart getStartChartFromDB
  if rl + length r < l && Fcd.date minc < s
    then do r' <- (++) <$> getChartListBack (s - l) l (rl + length r) <*> pure r
            if 0 < length r' - l
              then return $ drop (length r' - l) r'
              else return r'
    else return r

getChartListForward :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListForward s l rl = do
  r <- getChartList s (s + l)
  maxc <- getOneChart getEndChartFromDB 
  if rl + length r < l && s < Fcd.date maxc
    then do r' <- (++) <$> pure r <*> getChartListForward (s + l) l (rl + length r)
            return $ take l r'
    else return r

getOneChart :: (ReaderT MongoContext IO [Document]) -> IO Fcd.FxChartData 
getOneChart f = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" f
  close pipe
  r' <- mapM (\x -> return $ Fcd.FxChartData { Fcd.date  = typed $ valueAt "time"  x
                                             , Fcd.open  = typed $ valueAt "open"  x
                                             , Fcd.high  = typed $ valueAt "high"  x
                                             , Fcd.low   = typed $ valueAt "low"   x
                                             , Fcd.close = typed $ valueAt "close" x
                                             }
             ) r
  return $ head r'

getChartList :: Int -> Int -> IO [Fcd.FxChartData]
getChartList s e = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getChartListFromDB  s e
  close pipe
  mapM (\x -> return $ Fcd.FxChartData { Fcd.date  = typed $ valueAt "time"  x
                                       , Fcd.open  = typed $ valueAt "open"  x
                                       , Fcd.high  = typed $ valueAt "high"  x
                                       , Fcd.low   = typed $ valueAt "low"   x
                                       , Fcd.close = typed $ valueAt "close" x
                                       }
       ) r

getChartListFromDB :: Int -> Int -> ReaderT MongoContext IO [Document]
getChartListFromDB s e = do
  rest =<< find (select ["time" =: ["$gte" =: s, "$lte" =: e]] "rate")

getStartChartFromDB :: ReaderT MongoContext IO [Document]
getStartChartFromDB = do
  rest =<< find (select [] "rate") {sort = ["time" =: (1 :: Int)], limit = 1}

getEndChartFromDB :: ReaderT MongoContext IO [Document]
getEndChartFromDB = do
  rest =<< find (select [] "rate") {sort = ["time" =: (-1 :: Int)], limit = 1}

setFxTradeData :: String-> Ftd.FxTradeData -> IO ()
setFxTradeData coName td = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setFxTradeDataToDB (T.pack coName) td
  close pipe

updateFxTradeData :: String -> Ftd.FxTradeData -> IO Ftd.FxTradeData
updateFxTradeData coName td = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB (T.pack coName)
  close pipe
  if r == []
    then return td
    else head <$> mapM (\x -> return $ td { Ftd.chart      = read . typed $ valueAt "td" x
                                          , Ftd.trSuccess  = typed $ valueAt "tr_success" x
                                          , Ftd.trFail     = typed $ valueAt "tr_fail" x
                                          , Ftd.profit     = typed $ valueAt "profit" x
                                          , Ftd.realizedPL = typed $ valueAt "realized_pl" x
                                          }) r

readFxSettingData :: IO (Fsd.FxSettingData)
readFxSettingData  = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB "fsd"
  close pipe
  if r == []
    then return $ Fsd.initFxSettingData 
    else do fls <- head <$> mapM (\x -> return $ (read . typed $ valueAt "fls" x)) r
            fsl <- head <$> mapM (\x -> return $ (read . typed $ valueAt "fsl" x)) r
            return $ Fs.setFxSettingData fls fsl

writeFxSettingData :: Fsd.FxSettingData -> IO ()
writeFxSettingData fsd = do
  let fls = Fsd.learningSetting fsd
      fsl = Fsd.fxSettingLog    fsd
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  _ <- access pipe master "fx" $ setFxSettingToDB fls fsl
  close pipe

getDataFromDB :: T.Text -> ReaderT MongoContext IO [Document]
getDataFromDB coName = do
  rest =<< find (select [] coName) 

setFxTradeDataToDB :: T.Text -> Ftd.FxTradeData -> Action IO Value
setFxTradeDataToDB coName td = do
  delete (select [] coName)
  insert coName [ "chart"       =: (show $ Ftd.chart td)
                , "tr_success"  =: Ftd.trSuccess td
                , "tr_fail"     =: Ftd.trFail td
                , "profit"      =: Ftd.profit td
                , "realized_pl" =: Ftd.realizedPL td
                ]

setFxSettingToDB :: Fsd.FxLearningSetting -> M.Map Fsd.FxSetting (Double, Int) -> Action IO Value
setFxSettingToDB fls fsl = do
  delete (select [] "fsd")
  insert "fsd" [ "fls"  =: (show $ fls)
               , "fsl"  =: (show $ fsl)
               ]
