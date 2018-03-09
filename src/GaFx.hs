module GaFx
  ( backTest
  , trade
  ) where

import qualified Ga 
import qualified GlobalSettingFunction    as Gsf
import qualified FxTrade                  as Ft
import qualified FxSetting                as Fs
import qualified FxSettingData            as Fsd
import qualified FxSetting                as Fs
import qualified FxTradeData              as Ftd
import qualified GlobalSettingData        as Gsd
import qualified FxChartData              as Fcd
import qualified FxPrint                  as Fp
import qualified FxMongodb                as Fm
import qualified FxOandaAPI               as Foa
import qualified FxTweet                  as Ftw
import qualified FxTime                   as Ftm
import qualified Data.Map                 as M
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Control.Monad.Random
import Debug.Trace

instance Ga.Ga Fsd.FxSettingData where
  reset             = Fs.resetFxSettingData
  copy              = Fs.copyFxSettingData
  mutation          = Fs.mutationFxSettingData
  crossover         = Fs.crossoverFxSettingData
  createInitialData = Fs.createInitialGaData
  learningEvaluate  = Ft.gaLearningEvaluate
  getGaLength       = Fsd.getGaLength  
  getGaLoopMax      = Fsd.getGaLoopMax 
  plusGaLength      = Fsd.plusGaLength 
  plusGaLoopMax     = Fsd.plusGaLoopMax

backTest :: IO ()
backTest = do
  startN <- Fcd.date <$> Fm.getStartChart
  fsd <- Fm.updateFxSettingData $ Fsd.initFxSettingData
  --let a = map fst $ Fsd.fxSettingLog fsd
  --traceShow(a) $ return ()
  backTestMainLoop startN 0 0 fsd 
  
backTestMainLoop :: Int -> Int -> Int -> Fsd.FxSettingData -> IO ()
backTestMainLoop start s f fsd = do
  startN <- Fcd.date <$> Fm.getStartChart
  endN <- Fcd.date <$> Fm.getEndChart
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      n   = start + Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd
  (fs, fsd') <- backTestLoop 0 n endN td fsd
  (s', f') <- if fs
              then do printf "================================= %d - %d \n" (s + 1) f 
                      return (s + 1, f)
              else do printf "--------------------------------- %d - %d \n" s (f + 1)
                      return (s, f + 1)
  start' <- getRandomR(startN, startN + ltt * 2)
  backTestMainLoop start' s' f' fsd'

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getEndChart 
  td <- Fm.updateFxTradeData coName =<< (Foa.updateFxTradeData $ Ft.initFxTradeData environment)
  let td' = td { Ftd.chart = c }
  fsd <- tradeLearningThread =<< (Fm.updateFxSettingData $ Fsd.initFxSettingData)
  Fp.printStartTrade td'
  -- Fp.printFxSettingData fsd
  tradeWeeklyLoop td' fsd False

learningLoop :: Int ->
                [Fcd.FxChartData] -> 
                [[Fcd.FxChartData]] ->
                Fsd.FxSettingData ->
                IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c cl ce fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  (_, tdlt, fsd') <- (maximum . map (\x -> let t = map (\y -> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                              Fsd.nextFxSettingData ltt y x) ce
                                               p = Gsf.getEvaluationValueList t
                                           in (p, t, x)) . Ga.getGaDataList) <$>
                     (Ga.learning (Fsd.nextFxSettingData lt cl fsd) . Ga.learningData $ Fsd.nextFxSettingData lt cl fsd)
  let tdl = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
  --Fp.printLearningFxTradeData fsd'' tdl tdlt
  --Fp.printFxSettingData xm
  if Gsf.evaluationOk tdl tdlt
    then return (0, True, tdl, tdlt, fsd')
    else if Fs.getLearningTestTimes fsd < fromIntegral c
         then return (0, False, tdl, tdlt, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) cl ce fsd'

learning :: Int ->
            Int ->
            Fsd.FxSettingData -> 
            IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning retry n fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  cl <-             Fm.getChartListBack (n - ltt * Gsd.learningTestCount Gsd.gsd) (Fs.getPrepareTimeAll fsd + lt ) 0
  ce <- mapM (\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd + ltt) 0) $ map (ltt *) [0..Gsd.learningTestCount Gsd.gsd- 1]
  let tdlts = M.elems . M.filter (\(_, x, _, _, _) -> x) $ 
              M.mapWithKey (\y (p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                             tdlt = map (\x-> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                         Fsd.nextFxSettingData ltt x fsd') ce
                                             tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                         in (Gsf.getEvaluationValueList tdlt * (p / fromIntegral c),
                                             Gsf.evaluationOk tdl tdlt, tdl, tdlt, fsd')) $
              Fsd.fxSettingLog fsd
  --Fp.printLearningFxTradeData fsd tdl tdlt
  if retry == 0 && (not $ null tdlts)
    then do let (_, _, tdl', tdlt', fsd'') = maximum tdlts
            return (length tdlts, True, tdl', tdlt', fsd'')
    else learningLoop 0 cl ce fsd

tradeLearningThread :: Fsd.FxSettingData ->
                       IO (Fsd.FxSettingData)
tradeLearningThread fsd = do
  printf "%s : learning\n" =<< Ftm.getLogTime
  e <- Fm.getEndChart 
  (pc, lsf, tdl, tdlt, fsd') <- learning 0 (Fcd.date e) fsd
  Fp.printLearningFxTradeData fsd' tdl tdlt pc lsf
  -- Fp.printFxSettingData fsd'
  printf "%s : learning done\n" =<< Ftm.getLogTime
  return (fsd')
  
backTestLoop :: Int ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop retry n endN td fsd  = do
  (pc, lsf, tdl, tdlt, fsd1) <- learning retry n fsd
  let ltt = Fs.getLearningTestTime fsd1
      sc = (foldl (\acc x -> acc + Ftd.trSuccess x) 0 tdlt)
  ctl <- Fm.getChartListBack    n (Fs.getPrepareTimeAll fsd1) 0
  ctt <- Fm.getChartListForward n (ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd) 0
  let ct = ctl ++ ctt
      (tdt, ctdl) = Ft.backTest (ltt * Gsd.learningTestCount Gsd.gsd) (Gsd.maxTradePeriod Gsd.gsd) sc td fsd1 ct
      n' = Fcd.date $ Ftd.chart tdt
  Fp.printTestProgress (Ftd.realizedPL tdt < Ftd.realizedPL td ) n n' fsd1 tdt tdl tdlt pc lsf
  fsd2 <- Fm.updateFxSettingData $ Fs.updateFxSettingData ctdl td tdt fsd1
  let tdt' = Ft.resetFxalgorithmListCount tdt
  if Ftd.realizedPL tdt < Ftd.realizedPL td 
    then if Fsd.getBacktestLoopMax fsd2 < retry
         then return (False, Fsd.plusBacktestLoopMax fsd2)
         else backTestLoop (retry + 1) n endN td $ Fsd.plusGaLength fsd2
    else if endN <= n' 
         then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt' , fsd2)
         else backTestLoop 0 n' endN tdt' fsd2
         
tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 [Fcd.FxChartData] ->
                 Bool ->
                 IO (Ftd.FxTradeData, Fsd.FxSettingData, Bool)
tradeEvaluate td fsd xcd pcs = do
  let (open, close, td1) = Ft.trade td fsd xcd
  (td3, fsd2, pcs' ) <- if close /= Ftd.None
                        then do td2 <- Foa.close td1
                                Fm.setFxTradeData "trade_practice" td2
                                Fp.printTradeResult td td2 0
                                return (td2, fsd, Ftd.profit td1 < Ftd.profit td2)
                        else return (td1, fsd, pcs)
  td5 <- if open /= Ftd.None
         then do (units, td4) <- Foa.open td3 open
                 Fm.setFxTradeData "trade_practice" td4
                 Fp.printTradeResult td td4 units
                 return td4
         else return td3
  return (td5, fsd2, pcs')

waitTrade :: IO ()
waitTrade = do
  waitTradeLoop =<< Fm.getEndChart 

waitTradeLoop :: Fcd.FxChartData -> IO ()
waitTradeLoop p = do
  e <- Fm.getEndChart 
  if e /= p
    then return ()
    else do threadDelay (60 * 1000 * 1000)
            waitTradeLoop e

tradeWeeklyLoop :: Ftd.FxTradeData ->
                   Fsd.FxSettingData ->
                   Bool ->
                   IO ()
tradeWeeklyLoop td fsd pcs = do
  waitTrade
  e <- Fm.getEndChart 
  (td', fsd', pcs') <- tradeLoop e 0 0 pcs td fsd =<< async (tradeLearningThread fsd)
  tds <- Fm.updateFxTradeData "trade_practice_weekly" td'
  Ftw.tweetWeek tds td'
  Fm.setFxTradeData "trade_practice_weekly" td'
  tradeWeeklyLoop td' fsd' pcs'

tradeLearning :: Async (Fsd.FxSettingData) ->
                 Fsd.FxSettingData ->
                 IO (Async (Fsd.FxSettingData), Fsd.FxSettingData)
tradeLearning a fsd = do
  e <- poll a
  case e of
    Nothing -> return (a, fsd)
    Just _  -> do fsd' <- wait a
                  a' <- async (tradeLearningThread fsd')
                  return (a', fsd')
                 
tradeLoop :: Fcd.FxChartData ->
             Int ->
             Int ->
             Bool ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             Async (Fsd.FxSettingData) ->
             IO (Ftd.FxTradeData, Fsd.FxSettingData, Bool)
tradeLoop p sleep lc pcs td fsd a = do
  (a', fsd1, lc') <- if 120 < lc && not pcs
                    then do (a', fsd') <- tradeLearning a fsd
                            return (a', fsd', 0)
                    else return (a, fsd, lc + 1)
  e <- Fm.getEndChart 
  ct <- Fm.getChartListBack (Fcd.date e) (Fs.getPrepareTimeAll fsd1 + 1) 0
  (sleep', td2, fsd4, pcs'' ) <- if last ct /= p
                                 then do (td1, fsd2, pcs') <- tradeEvaluate td fsd1 ct pcs
                                         return  (0, td1, fsd2, pcs')
                                 else return (sleep + 1, td, fsd1, pcs)
  t <- getCurrentTime
  threadDelay ((60 - ((truncate $ utcTimeToPOSIXSeconds t) `mod` 60)) * 1000 * 1000)
  if 30 < sleep' 
    then do ix' <- wait a'
            return (td2, ix', pcs'')
    else tradeLoop (last ct) sleep' lc' pcs'' td2 fsd4 a'
    
    


