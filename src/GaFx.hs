module GaFx
  ( backTest
  , trade
  ) where

import qualified Ga 
import qualified FxTrade                  as Ft
import qualified FxSetting                as Fs
import qualified FxSettingData            as Fsd
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
  let td  = Ftd.initFxTradeData Ftd.Backtest
      ltt = Fsd.getLearningTestTime fsd
      lt  = Fsd.getLearningTime fsd
      n   = start + Fsd.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd * 2
  (fs, fsd') <- backTestLoop n endN td fsd
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
  td <- Fm.updateFxTradeData coName =<< (Foa.updateFxTradeData $ Ftd.initFxTradeData environment)
  let td' = td { Ftd.chart = c }
  fsd <- tradeLearningThread =<< (Fm.updateFxSettingData $ Fsd.initFxSettingData)
  Fp.printStartTrade td'
  -- Fp.printFxSettingData fsd
  tradeWeeklyLoop td' fsd False

learningLoop :: Int ->
                Double -> 
                [Fcd.FxChartData] -> 
                [[Fcd.FxChartData]] ->
                Fsd.FxSettingData ->
                IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c ptp cl ce fsd = do
  let lt  = Fsd.getLearningTime     fsd
      ltt = Fsd.getLearningTestTime fsd
  xs' <- Ga.learning (Fsd.nextFxSettingData lt cl fsd) . Ga.learningData $ Fsd.nextFxSettingData lt cl fsd
  let (tp, tdlt, fsd') = maximum $ map (\x -> let t = map (\y -> Ft.learning (Ftd.initFxTradeData Ftd.Backtest) $
                                                                 Fsd.nextFxSettingData ltt y x) ce
                                                  p = Ftd.getEvaluationValueList t
                                              in (p, t, x)) $ Ga.getGaDataList xs'
      tdl = Ft.learning (Ftd.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
  --Fp.printLearningFxTradeData fsd'' tdl tdlt
  --Fp.printFxSettingData xm
  if Ftd.evaluationOk tdl tdlt
    then return (0, True, tdl, tdlt, fsd')
    else if fsd == fsd' || Fsd.getLearningTestTimes fsd' < fromIntegral c || ptp <= tp
         then return (0, False, tdl, tdlt, Fsd.plusGaLength $ Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) tp cl ce fsd'

learning :: Int ->
            Fsd.FxSettingData -> 
            IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  let lt  = Fsd.getLearningTime     fsd
      ltt = Fsd.getLearningTestTime fsd
  cl <-             Fm.getChartListBack (n - ltt * Gsd.learningTestCount Gsd.gsd) (Fsd.getPrepareTimeAll fsd + lt ) 0
  ce <- mapM (\x -> Fm.getChartListBack (n - x) (Fsd.getPrepareTimeAll fsd + ltt) 0) $ map (ltt *) [0..Gsd.learningTestCount Gsd.gsd- 1]
  let tdlts = M.elems . M.filter (\(_, x, _, _, _) -> x) $ 
              M.mapWithKey (\y (p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                             tdlt = map (\x-> Ft.learning (Ftd.initFxTradeData Ftd.Backtest) $
                                                         Fsd.nextFxSettingData ltt x fsd') ce
                                             tdl  = Ft.learning (Ftd.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                         in (Ftd.getEvaluationValueList tdlt * (p / fromIntegral c),
                                             Ftd.evaluationOk tdl tdlt, tdl, tdlt, fsd')) $
              Fsd.fxSettingLog fsd
      (_, _, tdl', tdlt', fsd'') = maximum tdlts
  --Fp.printLearningFxTradeData fsd tdl tdlt
  if not $ null tdlts 
    then return (length tdlts, True, tdl', tdlt', fsd'')
    else learningLoop 0 0 cl ce fsd

tradeLearningThread :: Fsd.FxSettingData ->
                       IO (Fsd.FxSettingData)
tradeLearningThread fsd = do
  printf "%s : learning\n" =<< Ftm.getLogTime
  e <- Fm.getEndChart 
  (pc, lsf, tdl, tdlt, fsd') <- learning (Fcd.date e) fsd
  Fp.printLearningFxTradeData fsd' tdl tdlt pc lsf
  -- Fp.printFxSettingData fsd'
  printf "%s : learning done\n" =<< Ftm.getLogTime
  return (fsd')
  
backTestLoop :: Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop n endN td fsd  = do
  (pc, lsf, tdl, tdlt, fsd1) <- learning n fsd
  let ltt = Fsd.getLearningTestTime fsd1
      sc = (foldl (\acc x -> acc + Ftd.trSuccess x) 0 tdlt)
  ctl <- Fm.getChartListBack    n (Fsd.getPrepareTimeAll fsd1) 0
  ctt <- Fm.getChartListForward n (ltt * 2 * Gsd.learningTestCount Gsd.gsd) 0
  let ct = ctl ++ ctt
      (tdt, ctdl) = Ft.backTest (ltt * 2 * Gsd.learningTestCount Gsd.gsd) sc td fsd1 ct
      n' = Fcd.date $ Ftd.chart tdt
  Fp.printTestProgress n n' fsd1 tdt tdl tdlt pc lsf
  fsd2 <- Fm.updateFxSettingData . Fsd.updateSettingLog td tdt $ Fsd.updateLearningSetting ctdl tdt fsd1
  let tdt' = Ftd.resetFxalgorithmListCount tdt
  if Ftd.realizedPL tdt' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (False, fsd2)
    else if endN <= n'
         then if Ftd.realizedPL tdt' < Gsd.initalProperty Gsd.gsd
              then return (False, fsd2)
              else return (True, fsd2)
         else backTestLoop n' endN tdt' fsd2
         
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
  ct <- Fm.getChartListBack (Fcd.date e) (Fsd.getPrepareTimeAll fsd1 + 1) 0
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
    
    


