module GaFx
  ( backTest
  , trade
  , tradeSim
  , statistics
  , debug
  ) where

import           Control.Concurrent
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import qualified FxChartData           as Fcd
import qualified FxMongodb             as Fm
import qualified FxRedis               as Fr
import qualified FxOandaAPI            as Foa
import qualified FxPrint               as Fp
import qualified FxSetting             as Fs
import qualified FxSettingData         as Fsd
import qualified FxTechnicalAnalysis   as Ta
import qualified FxTrade               as Ft
import qualified FxTradeData           as Ftd
import qualified FxTweet               as Ftw
import qualified Ga
import qualified GlobalSettingData     as Gsd
import           Text.Printf

statistics :: IO ()
statistics = do
  let td = Ft.initFxTradeDataBacktest
  fsd <- Fm.readFxSettingData
  Prelude.mapM (\(p, c) -> printf "%f %d\n" p c) $ Fsd.fxSettingLog fsd
  return ()

debug :: IO ()
debug = do
  return ()

backTest :: IO ()
backTest = do
  fsd <- Fm.readFxSettingData
  let td = Ft.initFxTradeDataBacktest
      startN = Gsd.maxTradeTime Gsd.gsd + (Ta.getLearningTestTime fsd + Ta.getPrepareTimeAll fsd) * Gsd.learningTestCount Gsd.gsd * 2
  (s, f) <- Fm.readBacktestResult "backtest"
  endN <- (-) <$> (Fcd.no <$> Fr.getEndChart) <*> pure 1
  (tdt, fsd') <- backTestLoop True startN endN td fsd
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt
              then do Fp.printBackTestResult "=================================" tdt (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" tdt s (f + 1) fsd'
                      return (s, f + 1)
  Fm.writeBacktestResult "backtest" s' f'
  backTest

trade :: Ftd.FxEnvironment -> IO ()
trade environment = do
  td <- Ft.initFxTradeDataTrade environment
  fsd <- Fm.readFxSettingData
  c <- Fr.getEndChart
  td <- Fm.updateFxTradeData (Ftd.coName td) td { Ftd.chart = c }
  td' <- Foa.updateFxTradeData Ftd.None 0 td td 
  Fp.printProgressFxTradeData td' c
  tradeWeeklyLoop td' fsd

tradeSim :: IO ()
tradeSim = do
  fsd <- Fm.readFxSettingData
  let td = Ft.initFxTradeDataBacktest
      startN = Gsd.maxTradeTime Gsd.gsd + (Ta.getLearningTestTime fsd + Ta.getPrepareTimeAll fsd) * Gsd.learningTestCount Gsd.gsd * 2
  endN <- (-) <$> (Fcd.no <$> Fr.getEndChart) <*> pure 1
  fsd <- tradeSimLearning startN fsd
  (s, f) <- Fm.readBacktestResult "trade_sim"
  td' <- tradeSimLoop startN endN td fsd
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL td'
              then do Fp.printBackTestResult "=================================" td' (s + 1) f fsd
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" td' s (f + 1) fsd
                      return (s, f + 1)
  Fm.writeBacktestResult "trade_sim" s' f'
  tradeSim

learningEvaluate :: Int ->
                    Ga.LearningData Fsd.FxSettingData ->
                    IO (Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningEvaluate n ld = do
  r <- Prelude.mapM (\fsd -> do tdlt <- Ft.learningEvaluate n fsd
                                let  p' = Ftd.getEvaluationValueList tdlt * (Fsd.getLogProfit fsd + 1)
                                return (p', Ft.evaluationOk tdlt fsd, tdlt, fsd)) $ Ga.getGaDataList ld
  let r' = L.filter (\(_, y, _, _) -> y) r
      (pOk, _, tdltmOk, fsdOk) = L.maximum r'
      (pNg, _, tdltmNg, fsdNg) = L.maximum r
  return $ if L.null r'
           then (False, 0,           tdltmNg, fsdNg)
           else (True,  L.length r', tdltmOk, fsdOk)

learningLoop :: Int ->
                Int ->
                Ga.LearningData Fsd.FxSettingData ->
                IO (Bool, Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c n ld = do
  ld' <- Ga.learning ld
  (ok, plok, tdltm, fsd) <- learningEvaluate n ld'
  if ok
    then return (False, True, plok, tdltm, Fsd.plusLearningTestTimes fsd)
    else if Fsd.getLearningTestTimes fsd < fromIntegral c || Ga.maximumScore ld == Ga.maximumScore ld'
         then return (False, False, plok, tdltm, Fsd.plusLearningTestTimes fsd)
         else learningLoop (c + 1) n ld'

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Bool, Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  ld <- Fs.gaLearningDataFromLog n fsd
  (ok, plok, tdltm, fsd') <- learningEvaluate n ld
  if ok
    then return (True, True, plok, tdltm, fsd')
    else learningLoop 0 n ld

tradeLearning :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
tradeLearning fsd = do
  e <- Fr.getEndChart
  (lok, ok, oknum, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdlt oknum lok ok
  return fsd'

backTestLoop :: Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop lf n endN td fsd = do
  (lok, ok, oknum, tdlt, fsd1) <- if lf 
                                  then learning n fsd
                                  else return (True, True, 0, [Ftd.initFxTradeDataCommon], fsd)
  (fsd2, tdt) <- Ft.backTest n td fsd1
  fsd3 <- Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2 <$> Fm.readFxSettingData
  Fp.printTestProgress fsd3 fsd td tdt tdlt oknum lok ok
  Fm.writeFxSettingData fsd3
  let n' = Fcd.no (Ftd.chart tdt) + 1
  if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (tdt, fsd3)
    else backTestLoop (Ftd.profit tdt < Ftd.profit td || (Ftd.profit tdt ==  Ftd.profit td && Ftd.side td == Ftd.None)) n' endN  tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 Fcd.FxChartData ->
                 IO (Ftd.FxTradeData, Fsd.FxSettingData)
tradeEvaluate td fsd e = do
  (open, close, td', fsd') <- Ft.trade td fsd e
  td'' <- if open /= Ftd.None && close /= Ftd.None
          then Foa.closeOpen td td'
          else if close /= Ftd.None
               then Foa.close td td'
               else if open /= Ftd.None
                    then Foa.open td td' open
                    else return td
  return(td'', fsd')

waitTrade :: IO ()
waitTrade =
  waitTradeLoop =<< Fr.getEndChart

waitTradeLoop :: Fcd.FxChartData -> IO ()
waitTradeLoop p = do
  e <- Fr.getEndChart
  if e /= p
    then return ()
    else do threadDelay (10 * 1000 * 1000)
            waitTradeLoop e

tradeWeeklyLoop :: Ftd.FxTradeData -> Fsd.FxSettingData ->
                   IO ()
tradeWeeklyLoop td fsd = do
  waitTrade
  fsd' <- tradeLearning fsd
  e <- Foa.getNowPrices td
  (td', fsd'') <- tradeLoop e 0 td fsd'
  tdw <- Fm.updateFxTradeData (Ftd.coName td L.++ "_weekly") td
  -- Ftw.tweetTWeek tdw td'
  Fm.setFxTradeData (Ftd.coName td L.++ "_weekly") td'
  tradeWeeklyLoop td' fsd''

tradeLoop :: Fcd.FxChartData ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             IO (Ftd.FxTradeData, Fsd.FxSettingData)
tradeLoop p sleep td fsd = do
  t <- getCurrentTime
  threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  e <- Foa.getNowPrices td
  (sleep', td'', fsd2) <- if Fcd.close e /= Fcd.close p
                          then do (td', fsd1) <- tradeEvaluate td fsd e
                                  return (0, td', fsd1)
                          else return (sleep + 1, td, fsd)
  fsd4 <- if Ftd.profit td'' < Ftd.profit td
          then do fsd3 <- Fs.updateFxSettingLog (Ftd.profit td'' - Ftd.profit td) fsd2 <$> Fm.readFxSettingData
                  tradeLearning fsd3
          else return fsd2
  if 240 < sleep'
    then return (td'', fsd4)
    else tradeLoop e sleep' td'' fsd4

tradeSimEvaluate :: Int ->
                    Ftd.FxTradeData ->
                    Fsd.FxSettingData ->
                    IO (Ftd.FxTradeData, Fsd.FxSettingData)
tradeSimEvaluate n td fsd = do
  e <- Fr.getOneChart n 
  (open, close, td', fsd') <- Ft.trade td fsd e
  if open /= Ftd.None || close /= Ftd.None
    then Fp.printTradeResult open close td td' $ Ftd.unit td'
    else return ()
  return (td', fsd')

tradeSimLearning :: Int -> Fsd.FxSettingData -> IO Fsd.FxSettingData
tradeSimLearning n fsd = do
  e <- Fr.getOneChart n
  (lok, ok, oknum, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdlt oknum lok ok
  return fsd'

tradeSimLoop :: Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO Ftd.FxTradeData
tradeSimLoop n endN td fsd = do
  (td', fsd1) <- tradeSimEvaluate n td fsd
  fsd3 <- if Ftd.profit td' < Ftd.profit td
          then do fsd2 <- Fs.updateFxSettingLog (Ftd.profit td' - Ftd.profit td) fsd1 <$> Fm.readFxSettingData
                  tradeSimLearning n fsd2
          else return fsd1
  if endN <= n ||  Ftd.realizedPL td' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return td'
    else tradeSimLoop (n + 1) endN td' fsd3
    

