module GaFx
  ( backTest
  , trade
  , statistics
  , test
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.DeepSeq
import qualified Data.Map                 as M
import qualified Data.List                as L
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Text.Printf
import           Data.Vector              as V
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
  fsd <- Fm.readFxSettingData "backtest"
  Prelude.mapM (\(p, c) -> do printf "%f %d\n" p c) $ Fsd.fxSettingLog fsd
  return ()

test :: IO ()
test = do
  let td = Ft.initFxTradeData Ftd.Practice
  e <- Foa.getNowPrices td
  print e
  
backTest :: IO ()
backTest = do
  fsd <- Fm.readFxSettingData "backtest"
  (s, f) <- Fm.readResult
  let td  = Ft.initFxTradeData Ftd.Backtest
      startN = Gsd.maxTradeTime Gsd.gsd * 12
  endN <- Fcd.no <$> Fm.getOneChart Fm.getEndChartFromDB
  -- fc <- V.fromList <$> Fm.getChartListAll
  let fc = V.empty
  (tdt, fsd') <- backTestLoop False startN endN fc td fsd
  Fm.writeFxSettingData "backtest" fsd'
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt
              then do Fp.printBackTestResult "=================================" (Ftd.realizedPL tdt) (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" (Ftd.realizedPL tdt) s (f + 1) fsd'
                      return (s, f + 1)
  Fm.writeResult s' f'
  backTest

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB
  td <- Foa.updateFxTradeData =<< (Fm.updateFxTradeData coName $ (Ft.initFxTradeData environment) { Ftd.chart = c })
  Fp.printProgressFxTradeData td c
  tradeWeeklyLoop td coName

learningEvaluate :: Int ->
                    Ga.LearningData Fsd.FxSettingData ->
                    IO (Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningEvaluate n ld = do
  r <- Prelude.mapM (\fsd -> do tdlt <- Ft.learning n fsd
                                let  p' = Ftd.getEvaluationValueList tdlt * (Fsd.getLogProfit fsd + 1)
                                return {- $ traceShow(Fsd.learningSetting $ Fsd.fxSetting fsd ,p' , Ftd.getEvaluationValueList tdlt, Ft.evaluationOk tdlt) $ -}
                                  (p', Ft.evaluationOk tdlt, tdlt, fsd)) $ Ga.getGaDataList ld
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
  ld' <- Ga.learning n (Fsd.getLearningTestTimes fsd) ld
  (ok, plok, tdltm, fsd') <- learningEvaluate n ld'
  if ok
    then return (False, True, plok, tdltm, fsd)
    else if Fsd.getLearningTestTimes fsd' < fromIntegral c || fsd == fsd'
         then return (False, False, plok, tdltm, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) n fsd' ld'

learning :: Int ->
            V.Vector Fcd.FxChartData ->
            Fsd.FxSettingData ->
            IO (Bool, Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fc fsd = do
  let ld = Fs.gaLearningDataFromLog fc fsd                 
  (ok, plok, tdltm, fsd) <- learningEvaluate n ld
  if ok
    then return (True, True, plok, tdltm, fsd)
    else learningLoop 0 n fsd ld
    
tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData "backtest"
  e <- Fm.getOneChart Fm.getEndChartFromDB
  fc <- V.fromList <$> Fm.getChartListAll
  (lok, ok, oknum, tdlt, fsd') <- learning (Fcd.no e) fc fsd
  Fp.printLearningFxTradeData fsd' tdlt oknum lok ok 
  return (Fcd.no e, fsd')

backTestLoop :: Bool ->
                Int ->
                Int -> 
                V.Vector Fcd.FxChartData ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop lf n endN fc td fsd = do
  (lok, ok, oknum, tdlt, fsd1) <- if Ftd.side td == Ftd.None || lf
                                  then learning n fc fsd
                                  else return (True, True, 0, [Ftd.initFxTradeDataCommon], fsd)
  (fsd2, tdt) <- Ft.backTest n fc td fsd1
  let fsd3 = Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2
  Fm.writeFxSettingData "backtest" fsd3
  Fp.printTestProgress fsd3 fsd td tdt tdlt oknum lok ok
  let n' = Fcd.no (Ftd.chart tdt) + 1
      ltt = Ta.getLearningTestTime fsd3 * Gsd.learningTestCount Gsd.gsd
  if endN <= n' + ltt || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (tdt, fsd3)
    else backTestLoop (Ftd.profit tdt <= Ftd.profit td) n' endN fc tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 String ->
                 Fcd.FxChartData ->
                 IO (Ftd.FxTradeData)
tradeEvaluate td fsd coName e = do
  (open, close, td1, _) <- Ft.trade td fsd e
  td3 <- if close /= Ftd.None
         then do td2 <- Foa.close td1
                 Fm.setFxTradeData coName td2
                 Fp.printTradeResult open close td td2 0
                 return td2
         else return td1
  td5 <- if open /= Ftd.None
         then do (units, td4) <- Foa.open td3 open
                 Fm.setFxTradeData coName td4
                 Fp.printTradeResult open close td td4 units
                 return td4
         else return td3
  return td5

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
                   String ->
                   IO ()
tradeWeeklyLoop td coName = do
  waitTrade
  (pl, fsd') <- tradeLearning
  e <- Foa.getNowPrices td
  td' <- tradeLoop e pl 0 td fsd' coName
  tdw <- Fm.updateFxTradeData (coName L.++ "_weekly") td
  -- Ftw.tweetTWeek tdw td'
  Fm.setFxTradeData (coName L.++ "_weekly") td'
  tradeWeeklyLoop td' coName

tradeLoop :: Fcd.FxChartData ->
             Int ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->            
             String ->
             IO Ftd.FxTradeData
tradeLoop p pl sleep td fsd coName = do
  t <- getCurrentTime
  threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  let ltt = Ta.getLearningTestTime fsd
  e <- Foa.getNowPrices td
  (pl', fsd1) <- if Ftd.side td == Ftd.None && ltt < Fcd.no e - pl
                 then tradeLearning
                 else return (pl, fsd)
  (sleep', td2) <- if (Fcd.close e) /= (Fcd.close p)
                         then do td1 <- tradeEvaluate td fsd1 coName e 
                                 -- Fp.printProgressFxTradeData td1 e                                 
                                 return (0, td1)
                   else return (sleep + 1, td) -- 
  if 240 < sleep'
    then return td2
    else tradeLoop e pl' sleep' td2 fsd1 coName

