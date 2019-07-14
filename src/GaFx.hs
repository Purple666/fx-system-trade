module GaFx
  ( backTest
  , trade
  , statistics
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
  mapM (\(p, c) -> do printf "%f %d\n" p c) $ Fsd.fxSettingLog fsd
  return ()

backTest :: String -> Bool -> Bool -> IO ()
backTest coName latest retry = do
  fsd <- Fm.readFxSettingData "backtest"
  (s, f) <- Fm.readResult coName
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Ta.getLearningTestTime fsd
      startN = 60 * 24 * 20 * 12 * 2
  endN <- Fcd.no <$> Fm.getOneChart Fm.getEndChartFromDB
  let n = if latest
          then endN - Gsd.backtestLatestTime Gsd.gsd 
          else startN
  (tdt, fsd') <- backTestLoop retry False n endN td fsd
  (s', f') <- if Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt
              then do Fp.printBackTestResult "=================================" (Ftd.realizedPL tdt) (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" (Ftd.realizedPL tdt) s (f + 1) fsd'
                      return (s, f + 1)
  Fm.writeResult coName s' f'
  backTest coName latest retry

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
  r <- mapM (\fsd -> do tdlt <- Ft.learning n fsd
                        let  p' = Ftd.getEvaluationValueList tdlt * (Fsd.getLogProfit fsd + 1)
                        return {- $ traceShow(Fsd.learningSetting $ Fsd.fxSetting fsd ,p' , Ftd.getEvaluationValueList tdlt, Ft.evaluationOk tdlt) $ -} 
                          (p', Ft.evaluationOk tdlt, tdlt, fsd)) $ Ga.getGaDataList ld
  let r' = filter (\(_, y, _, _) -> y) r
      (pOk, _, tdltmOk, fsdOk) = maximum r'
      (pNg, _, tdltmNg, fsdNg) = maximum r
  return $ if null r'
           then (False, 0,         tdltmNg, Fsd.plusLearningTestTimes fsdNg)
           else (True,  length r', tdltmOk, fsdOk)

learningLoop :: Int ->
                Int ->
                Fsd.FxSettingData ->
                Ga.LearningData Fsd.FxSettingData ->
                IO (Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c n fsd ld = do
  ld' <- Ga.learning n (Fsd.getLearningTestTimes fsd) ld
  (ok, plok, tdltm, fsd') <- learningEvaluate n ld'
  if ok
    then return (True, plok, tdltm, fsd)
    else if Fsd.getLearningTestTimes fsd' < fromIntegral c || fsd == fsd'
         then return (False, plok, tdltm, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) n fsd' ld'

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Bool, Int, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  let ld = Fs.gaLearningDataFromLog fsd                 
  (ok, plok, tdltm, fsd) <- learningEvaluate n ld
  if ok
    then return (True, plok, tdltm, fsd)
    else learningLoop 0 n fsd ld
    
tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData "backtest"
  e <- Fm.getOneChart Fm.getEndChartFromDB
  (ok, plok, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData 0 (Fcd.no e) fsd' tdlt plok ok
  return (Fcd.no e, fsd')

backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop retry lf n endN td fsd = do
  (ok, plok, tdlt, fsd1) <- if Ftd.side td == Ftd.None || lf
                               then learning n fsd
                               else return (True, 0, [Ftd.initFxTradeDataCommon], fsd)
  (fsd2, tdt) <- Ft.backTest n td fsd1
  fsd3 <- Fm.writeFxSettingData "backtest"
          <$> Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2
          =<< Fm.readFxSettingData "backtest"
  if Ftd.profit tdt <= Ftd.profit td && retry
    then do Fp.printTestProgress fsd1 fsd td tdt tdlt plok ok True
            backTestLoop retry True n endN td fsd3
    else do Fp.printTestProgress fsd1 fsd td tdt tdlt plok ok False
            let n' = Fcd.no (Ftd.chart tdt) + 1
            if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
              then return (tdt, fsd3)
              else backTestLoop retry (Ftd.profit tdt <= Ftd.profit td) n' endN tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 String ->
                 Fcd.FxChartData ->
                 IO (Ftd.FxTradeData)
tradeEvaluate td fsd coName e = do
  (open, close, td1) <- Ft.trade td fsd e
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
  tdw <- Fm.updateFxTradeData (coName ++ "_weekly") td
  -- Ftw.tweetTWeek tdw td'
  Fm.setFxTradeData (coName ++ "_weekly") td'
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
                   else return (sleep + 1, td)
  if 240 < sleep'
    then do return td2
    else tradeLoop e pl' sleep' td2 fsd1 coName

