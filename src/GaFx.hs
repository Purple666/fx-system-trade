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
  c <- Fr.getChartList 600000 10
  debug

backTest :: IO ()
backTest = do
  fsd <- Fm.readFxSettingData
  let td = Ft.initFxTradeDataBacktest
      startN = Gsd.maxTradeTime Gsd.gsd * 2
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
  c <- Fr.getEndChart
  td <- Fm.updateFxTradeData (Ftd.coName td) td { Ftd.chart = c }
  td' <- Foa.updateFxTradeData Ftd.None 0 td td 
  Fp.printProgressFxTradeData td' c
  tradeWeeklyLoop td'

tradeSim :: IO ()
tradeSim = do
  let td = Ft.initFxTradeDataBacktest
  endN <- (-) <$> (Fcd.no <$> Fr.getEndChart) <*> pure 1
  let startN = Gsd.maxTradeTime Gsd.gsd * 2
  (pl, fsd) <- tradeSimLearning startN
  (s, f) <- Fm.readBacktestResult "trade_sim"
  td' <- tradeSimLoop startN endN pl td fsd
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
                                return (p', Ft.evaluationOk tdlt, tdlt, fsd)) $ Ga.getGaDataList ld
  let r' = L.filter (\(_, y, _, _) -> y) r
      (pOk, _, tdltmOk, fsdOk) = L.maximum r'
      (pNg, _, tdltmNg, fsdNg) = L.maximum r
  return $ if L.null r'
           then (False, 0,           tdltmNg, fsdNg)
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
  (ok, plok, tdltm, fsd') <- learningEvaluate n ld
  if ok
    then return (True, True, plok, tdltm, fsd')
    else learningLoop 0 n fsd' ld'

tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData
  e <- Fr.getEndChart
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
  (lok, ok, oknum, tdlt, fsd1) <- if lf 
                                  then learning n fsd
                                  else return (True, True, 0, [Ftd.initFxTradeDataCommon], fsd)
  (fsd2, tdt) <- Ft.backTest n td fsd1
  let fsd3 = Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2
  Fp.printTestProgress fsd3 fsd td tdt tdlt oknum lok ok
  Fm.writeFxSettingData fsd3
  let n' = Fcd.no (Ftd.chart tdt) + 1
  if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (tdt, fsd3)
    else backTestLoop (Ftd.profit tdt < Ftd.profit td || (Ftd.profit tdt ==  Ftd.profit td && Ftd.side td == Ftd.None)) n' endN  tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 Fcd.FxChartData ->
                 IO Ftd.FxTradeData
tradeEvaluate td fsd e = do
  (open, close, td', _) <- Ft.trade td fsd e
  if open /= Ftd.None && close /= Ftd.None
    then Foa.closeOpen td td'
    else if close /= Ftd.None
         then Foa.close td td'
         else if open /= Ftd.None
              then Foa.open td td' open
              else return td

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
  (sleep', td'') <- if Fcd.close e /= Fcd.close p
                    then do td' <- tradeEvaluate td fsd' e
                            return (0, td')
                    else return (sleep + 1, td)
  if 240 < sleep'
    then return td''
    else tradeLoop e pl' sleep' td'' fsd'


tradeSimEvaluate :: Ftd.FxTradeData ->
                    Fsd.FxSettingData ->
                    Fcd.FxChartData ->
                    IO Ftd.FxTradeData
tradeSimEvaluate td fsd e = do
  (open, close, td', _) <- Ft.trade td fsd e
  if open /= Ftd.None || close /= Ftd.None
    then Fp.printTradeResult open close td td' $ Ftd.unit td'
    else return ()
  return td'

tradeSimLearning :: Int -> IO (Int, Fsd.FxSettingData)
tradeSimLearning n = do
  fsd <- Fm.readFxSettingData
  e <- Fr.getOneChart n
  (lok, ok, oknum, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdlt oknum lok ok
  return (Fcd.no e, fsd')

tradeSimLoop :: Int ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO Ftd.FxTradeData
tradeSimLoop n endN pl td fsd = do
  let ltt = Ta.getLearningTestTime fsd
  e <- Fr.getOneChart n 
  (pl', fsd') <- if ltt < Fcd.no e - pl
                 then tradeSimLearning n
                 else return (pl, fsd)
  td' <- tradeSimEvaluate td fsd' e
  if endN <= n ||  Ftd.realizedPL td' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return td'
    else tradeSimLoop (n + 1) endN pl' td' fsd'

