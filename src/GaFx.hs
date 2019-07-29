module GaFx
  ( backTest
  , trade
  , statistics
  , debug
  ) where

import           Control.Concurrent
import qualified Data.Map                 as M
import qualified Data.List                as L
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Text.Printf
import qualified FxChartData              as Fcd
import qualified FxMongodb                as Fm
import qualified FxOandaAPI               as Foa
import qualified FxPrint                  as Fp
import qualified FxSetting                as Fs
import qualified FxSettingData            as Fsd
import qualified FxTrade                  as Ft
import qualified FxTradeData              as Ftd
import qualified FxTweet                  as Ftw
import qualified Ga
import qualified GlobalSettingData        as Gsd
import qualified FxTechnicalAnalysis      as Ta

statistics :: IO ()
statistics = do
  let td = Ft.initFxTradeData Ftd.Backtest
  fsd <- Fm.readFxSettingData
  Prelude.mapM (\(p, c) -> do printf "%f %d\n" p c) $ Fsd.fxSettingLog fsd
  return ()

debug :: IO ()
debug = do
  let td = Ft.initFxTradeData Ftd.Practice
  Foa.closeOpen td 
  return ()
  
backTest :: IO ()
backTest = do
  let td  = Ft.initFxTradeData Ftd.Backtest
      startN = Gsd.maxTradeTime Gsd.gsd * 12
  fsd <- Fm.readFxSettingData
  (s, f) <- Fm.readBacktestResult
  endN <- Fcd.no <$> Fm.getOneChart Fm.getEndChartFromDB
  (tdt, fsd') <- backTestLoop False startN endN td fsd
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt
              then do Fp.printBackTestResult "=================================" (Ftd.realizedPL tdt) (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" (Ftd.realizedPL tdt) s (f + 1) fsd'
                      return (s, f + 1)
  Fm.writeBacktestResult s' f'
  backTest

trade :: Ftd.FxEnvironment -> IO ()
trade environment = do
  let td = Ft.initFxTradeData environment
  c <- Fm.getOneChart Fm.getEndChartFromDB
  td <- Foa.updateFxTradeData =<< (Fm.updateFxTradeData (Ftd.coName td) td { Ftd.chart = c })
  Fp.printProgressFxTradeData td c
  tradeWeeklyLoop td

learningEvaluate :: Int ->
                    Ga.LearningData Fsd.FxSettingData ->
                    IO (Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningEvaluate n ld = do
  r <- Prelude.mapM (\fsd -> do tdlt <- Ft.learningEvaluate n fsd
                                let  p' = Ftd.getEvaluationValueList tdlt * (Fsd.getLogProfit fsd + 1)
                                return (p', Ft.evaluationOk tdlt, tdlt, fsd)) $ Ga.getGaDataList ld
  let r' = L.filter (\(_, y, _, _) -> y) r
      (pOk, _, tdltmOk, fsdOk) = L.maximum r'
      (pNg, _, tdltmNg, fsdNg) = L.maximum r
  return $ if L.null r'
           then (False, 0,          tdltmNg, fsdNg)
           else (True,  L.length r', tdltmOk, fsdOk)

learningLoop :: Int ->
                Int ->
                Fsd.FxSettingData ->
                Ga.LearningData Fsd.FxSettingData ->
                IO (Bool, Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c n fsd ld = do
  ld' <- Ga.learning ld
  (ok, plok, tdltm, fsd') <- learningEvaluate n ld'
  if ok
    then return (False, True, plok, tdltm, Fsd.plusLearningTestTimes fsd')
    else if Fsd.getLearningTestTimes fsd' < fromIntegral c || fsd == fsd'
         then return (False, False, plok, tdltm, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) n fsd' ld'

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Bool, Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  (ld, ld') <- Fs.gaLearningDataFromLog n fsd
  (ok, plok, tdltm, fsd) <- learningEvaluate n ld
  if ok
    then return (True, True, plok, tdltm, fsd)
    else learningLoop 0 n fsd ld'
    
tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData
  e <- Fm.getOneChart Fm.getEndChartFromDB
  (lok, ok, oknum, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdlt oknum lok ok 
  return (Fcd.no e, fsd')

backTestLoop :: Bool ->
                Int ->
                Int -> 
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop lf n endN td fsd = do
  (lok, ok, oknum, tdlt, fsd1) <- if Ftd.side td == Ftd.None || lf
                                  then learning n fsd
                                  else return (True, True, 0, [Ftd.initFxTradeDataCommon], fsd)
{-
  (lok, ok, oknum, tdlt, fsd1) <- learning n fsd
-}
  (fsd2, tdt) <- Ft.backTest n td fsd1
  let fsd3 = Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2
  Fm.writeFxSettingData fsd3
  Fp.printTestProgress fsd3 fsd td tdt tdlt oknum lok ok
  let n' = Fcd.no (Ftd.chart tdt) + 1
  if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (tdt, fsd3)
    else backTestLoop (Ftd.profit tdt < Ftd.profit td) n' endN tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 Fcd.FxChartData ->
                 IO (Ftd.FxTradeData)
tradeEvaluate td fsd e = do
  (open, close, td1, _) <- Ft.trade td fsd e
  if open /= Ftd.None && close /= Ftd.None
    then Foa.closeOpen td1
    else if close /= Ftd.None
         then Foa.close td1
         else if open /= Ftd.None
              then Foa.open td1 open
              else return td1

waitTrade :: IO ()
waitTrade =
  waitTradeLoop =<< Fm.getOneChart Fm.getEndChartFromDB

waitTradeLoop :: Fcd.FxChartData -> IO ()
waitTradeLoop p = do
  e <- Fm.getOneChart Fm.getEndChartFromDB
  if e /= p
    then return ()
    else do threadDelay (10 * 1000 * 1000)
            waitTradeLoop e

tradeWeeklyLoop :: Ftd.FxTradeData ->
                   IO ()
tradeWeeklyLoop td = do
  waitTrade
  (pl, fsd') <- tradeLearning
  e <- Foa.getNowPrices td
  td' <- tradeLoop e pl 0 td fsd'
  tdw <- Fm.updateFxTradeData (Ftd.coName td L.++ "_weekly") td
  -- Ftw.tweetTWeek tdw td'
  Fm.setFxTradeData (Ftd.coName td L.++ "_weekly") td'
  tradeWeeklyLoop td'

tradeLoop :: Fcd.FxChartData ->
             Int ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->            
             IO Ftd.FxTradeData
tradeLoop p pl sleep td fsd = do
  t <- getCurrentTime
  threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  let ltt = Ta.getLearningTestTime fsd
  e <- Foa.getNowPrices td
  (pl', fsd') <- if ltt < Fcd.no e - pl
                 then tradeLearning
                 else return (pl, fsd)
  (sleep', td'') <- if (Fcd.close e) /= (Fcd.close p)
                         then do td' <- tradeEvaluate td fsd' e 
                                 return (0, td')
                   else return (sleep + 1, td)
  if 240 < sleep'
    then return td''
    else tradeLoop e pl' sleep' td'' fsd' 

