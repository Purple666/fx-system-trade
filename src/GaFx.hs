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
  (tdt, fsd') <- backTestLoop startN endN td fsd
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
  td' <- tradeSimLoop startN startN endN td fsd
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL td'
              then do Fp.printBackTestResult "=================================" td' (s + 1) f fsd
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" td' s (f + 1) fsd
                      return (s, f + 1)
  Fm.writeBacktestResult "trade_sim" s' f'
  tradeSim

learningEvaluate :: Int ->
                    Ga.LearningData Fsd.FxSettingData ->
                    IO (Bool, Int, Ftd.FxTradeData, Fsd.FxSettingData)
learningEvaluate n ld = do
  r <- Prelude.mapM (\fsd -> do tdl <- Ft.learningEvaluate n fsd
                                let p = Ft.getEvaluationValue fsd tdl
                                return (p, Ft.evaluationOk tdl fsd, tdl, fsd)) $ Ga.getGaDataList ld
  let r' = L.filter (\(_, y, _, _) -> y) r
      (_, _, tdlOk, fsdOk) = L.maximumBy (\(p0, _, _, _) (p1, _, _, _) -> compare p0 p1) r'
      (_, _, tdlNg, fsdNg) = L.maximumBy (\(p0, _, _, _) (p1, _, _, _) -> compare p0 p1) r
  return $ if L.null r'
           then (False, 0,           tdlNg, fsdNg)
           else (True,  L.length r', tdlOk, fsdOk)

learningLoop :: Int ->
                Int ->
                Ga.LearningData Fsd.FxSettingData ->
                IO (Bool, Bool, Int, Ftd.FxTradeData, Fsd.FxSettingData)
learningLoop c n ld = do
  ld' <- Ga.learning ld
  (ok, plok, tdl, fsd) <- learningEvaluate n ld'
  if ok
    then return (False, True, plok, tdl, Fsd.plusLearningTestTimes fsd)
    else if Fsd.getLearningTestTimes fsd < fromIntegral c || Ga.maximumScore ld == Ga.maximumScore ld'
         then return (False, False, plok, tdl, Fsd.plusLearningTestTimes fsd)
         else learningLoop (c + 1) n ld'

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Bool, Bool, Int, Ftd.FxTradeData, Fsd.FxSettingData)
learning n fsd = do
  ld <- Fs.gaLearningDataFromLog n fsd
  (ok, plok, tdl, fsd') <- learningEvaluate n ld
  if ok
    then return (True, True, plok, tdl, fsd')
    else learningLoop 0 n ld

tradeLearning :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
tradeLearning fsd = do
  e <- Fr.getEndChart
  (lok, ok, oknum, tdl, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdl oknum lok ok
  return fsd'

backTestLoop :: Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop n endN td fsd = do
  (lok, ok, oknum, tdl, fsd2) <- learning n fsd
  (fsd3, tdt) <- Ft.backTest n td fsd2
  fsd4 <- Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd3 <$> Fm.readFxSettingData
  Fm.writeFxSettingData fsd4
  Fp.printTestProgress fsd4 fsd td tdt tdl oknum lok ok
  let n' = Fcd.no (Ftd.chart tdt) + 1
  if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (tdt, fsd4)
    else backTestLoop n' endN  tdt fsd4

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
  (td', fsd'') <- tradeLoop e (Fcd.no e) (Fcd.no e) td fsd'
  tdw <- Fm.updateFxTradeData (Ftd.coName td L.++ "_weekly") td
  -- Ftw.tweetTWeek tdw td'
  Fm.setFxTradeData (Ftd.coName td L.++ "_weekly") td'
  tradeWeeklyLoop td' fsd''

tradeLoop :: Fcd.FxChartData ->
             Int ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             IO (Ftd.FxTradeData, Fsd.FxSettingData)
tradeLoop pc p sleep td fsd = do
  t <- getCurrentTime
  threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  e <- Foa.getNowPrices td
  (sleep', td'', fsd2) <- if Fcd.close e /= Fcd.close pc
                          then do (td', fsd1) <- tradeEvaluate td fsd e
                                  return (0, td', fsd1)
                          else return (sleep + 1, td, fsd)
  let ltt = Ta.getLearningTestTime fsd2 * Gsd.learningTestCount Gsd.gsd
  (p', fsd3) <- if Ftd.profit td'' < Ftd.profit td || ltt < (Fcd.no e) - p 
                then do fsd3 <- tradeLearning fsd2
                        return (Fcd.no e, fsd3)
                else return (p, fsd2)
  fsd5 <- if Ftd.profit td'' /= Ftd.profit td
          then do fsd4 <- Fs.updateFxSettingLog (Ftd.profit td'' - Ftd.profit td) fsd3 <$> Fm.readFxSettingData
                  Fm.writeFxSettingData fsd4
          else return fsd2
  if 240 < sleep'
    then return (td'', fsd5)
    else tradeLoop e p' sleep' td'' fsd5

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
  (lok, ok, oknum, tdl, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData fsd' tdl oknum lok ok
  return fsd'

tradeSimLoop :: Int ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO Ftd.FxTradeData
tradeSimLoop n p endN td fsd = do
  (td', fsd1) <- tradeSimEvaluate n td fsd
  let ltt = Ta.getLearningTestTime fsd1 * Gsd.learningTestCount Gsd.gsd
  (p', fsd3) <- if Ftd.profit td' < Ftd.profit td || ltt < n - p
                then do fsd2 <- tradeSimLearning n fsd1
                        return (n, fsd2)
                else return (p, fsd1)
  fsd4 <- if Ftd.profit td' /= Ftd.profit td
          then do fsd4 <- Fs.updateFxSettingLog (Ftd.profit td' - Ftd.profit td) fsd3 <$> Fm.readFxSettingData
                  Fm.writeFxSettingData fsd4
          else return fsd3
  if endN <= n ||  Ftd.realizedPL td' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return td'
    else tradeSimLoop (n + 1) p' endN td' fsd4
    

