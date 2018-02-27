{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module FxMongodb
  ( getChartListBack
  , getChartListForward
  , getStartChart
  , getEndChart
  , setFxTradeData
  , updateFxTradeData
  , updateFxSettingData
  ) where


import Database.MongoDB
import Control.Monad.Trans.Reader
--import Debug.Trace
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified GlobalSettingData        as Gsd
import qualified FxTradeData              as Ftd


getChartListBack :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListBack s l rl = do
  r <- getChartList (s - l) s
  minc <- getStartChart 
  if rl + length r < l && Fcd.date minc < s
    then do r' <- (++) <$> getChartListBack (s - l) l (rl + length r) <*> pure r
            if 0 < length r' - l
              then return $ drop (length r' - l) r'
              else return r'
    else return r

getChartListForward :: Int -> Int -> Int -> IO [Fcd.FxChartData]
getChartListForward s l rl = do
  r <- getChartList s (s + l)
  maxc <- getEndChart 
  if rl + length r < l && s < Fcd.date maxc
    then do r' <- (++) <$> pure r <*> getChartListForward (s + l) l (rl + length r)
            return $ take l r'
    else return r

getStartChart :: IO Fcd.FxChartData
getStartChart = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getStartChartFromDB 
  close pipe
  r' <- mapM (\x -> return $ Fcd.FxChartData { Fcd.date = typed $ valueAt "time" x
                                             , Fcd.close = typed $ valueAt "bid" x} ) r
  return $ head r'

getEndChart :: IO Fcd.FxChartData
getEndChart = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getEndChartFromDB 
  close pipe
  r' <- mapM (\x -> return $ Fcd.FxChartData { Fcd.date = typed $ valueAt "time" x
                                             , Fcd.close = typed $ valueAt "bid" x} ) r
  return $ head r'

getChartList :: Int -> Int -> IO [Fcd.FxChartData]
getChartList s e = do
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getChartListFromDB  s e
  close pipe
  mapM (\x -> return $ Fcd.FxChartData { Fcd.date = typed $ valueAt "time" x
                                       , Fcd.close = typed $ valueAt "bid" x} ) r

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
    else head <$> mapM (\x -> return $ td { Ftd.chart      = Fcd.FxChartData { Fcd.date  = typed $ valueAt "chart.date"  x
                                                                             , Fcd.close = typed $ valueAt "chart.close" x
                                                                             }
                                          , Ftd.trSuccess     = typed $ valueAt "tr_success" x
                                          , Ftd.trFail        = typed $ valueAt "tr_fail" x
                                          , Ftd.profit        = typed $ valueAt "profit" x
                                          , Ftd.realizedPL    = typed $ valueAt "realized_pl" x
                                          }) r

updateFxSettingData :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
updateFxSettingData fsd = do
  let fls = Fsd.learningSetting fsd
      fts = Fsd.fxSetting       fsd
      fsl = Fsd.fxSettingLog    fsd
  pipe <- connect (host $ Gsd.dbHost Gsd.gsd)
  r <- access pipe master "fx" $ getDataFromDB "fsd"
  fsd' <- if r == []
                   then do _ <- access pipe master "fx" $ setFxSettingToDB fls fts fsl
                           return (fsd)
                   else do fls' <- head <$> mapM (\x -> return $ (read . typed $ valueAt "fls" x)) r
                           fts' <- head <$> mapM (\x -> return $ (read . typed $ valueAt "fts" x)) r
                           fsl' <- head <$> mapM (\x -> return $ (read . typed $ valueAt "fsl" x)) r
                           if Fsd.trSuccess fls < Fsd.trSuccess fls'
                             then do return fsd { Fsd.learningSetting = fls'
                                                , Fsd.fxSetting       = Fsd.setFxSetting fts'
                                                , Fsd.fxSettingLog    = fsl'
                                                }
                             else do _ <- access pipe master "fx" $ setFxSettingToDB fls fts fsl
                                     return (fsd)
  close pipe
  return fsd'

getDataFromDB :: T.Text -> ReaderT MongoContext IO [Document]
getDataFromDB coName = do
  rest =<< find (select [] coName) 

setFxTradeDataToDB :: T.Text -> Ftd.FxTradeData -> Action IO Value
setFxTradeDataToDB coName td = do
  delete (select [] coName)
  insert coName [ "chart"       =: [ "date"  =: (Fcd.date  $ Ftd.chart td)
                                   , "close" =: (Fcd.close $ Ftd.chart td)
                                   ]
                , "tr_success"      =: Ftd.trSuccess td
                , "tr_fail"         =: Ftd.trFail td
                , "profit"          =: Ftd.profit td
                , "realized_pl"     =: Ftd.realizedPL td
                ]

setFxSettingToDB :: Fsd.FxLearningSetting -> Fsd.FxSetting -> M.Map Fsd.FxSetting (Double, Int) -> Action IO Value
setFxSettingToDB fls fts fsl = do
  delete (select [] "fsd")
  insert "fsd" [ "fls"  =: (show $ fls)
               , "fts"  =: (show $ fts)
               , "fsl"  =: (show $ fsl)
               ]
