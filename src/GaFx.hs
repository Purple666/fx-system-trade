module GaFx
  ( backTest
  , trade
  ) where

import qualified Ga 
import qualified GlobalSettingFunction    as Gsf
import qualified FxTrade                  as Ft
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
  plusGaLoopMax     = Fsd.plusGaLoopMax

backTest :: Bool -> IO ()
backTest retry = do
  startN <- Fcd.date <$> Fm.getOneChart Fm.getStartChartFromDB
  fsd <- Fm.readFxSettingData True $ Fsd.initFxSettingData
  backTestMainLoop retry startN 0 0 fsd 
  
backTestMainLoop :: Bool -> Int -> Int -> Int -> Fsd.FxSettingData -> IO ()
backTestMainLoop retry start s f fsd = do
  startN <- Fcd.date <$> Fm.getOneChart Fm.getStartChartFromDB
  endN <- Fcd.date <$> Fm.getOneChart Fm.getEndChartFromDB 
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      n   = start + Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd
  (fs, fsd') <- backTestLoop retry False n endN td fsd
  fsd'' <- Fm.readFxSettingData True fsd'
  (s', f') <- if fs
              then do printf "================================= %d - %d \n" (s + 1) f 
                      return (s + 1, f)
              else do printf "--------------------------------- %d - %d \n" s (f + 1)
                      return (s, f + 1)
  start' <- getRandomR(startN, startN + ltt * 2)
  backTestMainLoop retry start' s' f' fsd''

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB 
  td <- Fm.updateFxTradeData coName =<< (Foa.updateFxTradeData $ Ft.initFxTradeData environment)
  let td' = td { Ftd.chart = c }
  fsd <- tradeLearningThread =<< (Fm.readFxSettingData True $ Fsd.initFxSettingData)
  Fp.printStartTrade td'
  tradeWeeklyLoop td' fsd False

learningLoop :: Int ->
                [Fcd.FxChartData] -> 
                [[Fcd.FxChartData]] ->
                Fsd.FxSettingData ->
                IO (Bool, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c cl ce fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  (_, tdlt, fsd') <- (maximum . map (\x -> let t = map (\y -> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                                          Fsd.nextFxSettingData ltt y x) ce
                                               p = Gsf.getEvaluationValueList t
                                           in (p, t, x)) . Ga.getGaDataList) <$>
                     (Ga.learning $ Fsd.nextFxSettingData lt cl fsd)
  let tdl = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
  --Fp.printLearningFxTradeData fsd'' tdl tdlt
  if Gsf.evaluationOk tdl tdlt
    then return (False, True, tdl, tdlt, fsd')
    else if Fs.getLearningTestTimes fsd' < fromIntegral c
         then return (False, False, tdl, tdlt, Fsd.plusGaLength $ Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) cl ce fsd'
              
learning :: Bool ->
            Int ->
            Fsd.FxSettingData -> 
            IO (Bool, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning fail n fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  cl <-             Fm.getChartListBack (n - ltt * Gsd.learningTestCount Gsd.gsd) (Fs.getPrepareTimeAll fsd + lt ) 0
  ce <- mapM (\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd + ltt) 0) $ map (ltt *) [0..Gsd.learningTestCount Gsd.gsd - 1]
  let tdlts = M.elems .
              M.filter (\(x, y, _, _, _) -> 0 < x && y) .
              M.mapWithKey (\y (p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                             tdlt = map (\x-> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                              Fsd.nextFxSettingData ltt x fsd') ce
                                             tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                         in (Gsf.getEvaluationValueList tdlt * (p / fromIntegral c), Gsf.evaluationOk tdl tdlt,
                                             tdl, tdlt, fsd')) $
              Fsd.fxSettingLog fsd
      (_, _, tdl', tdlt', fsd'') = maximum tdlts
  -- Fp.printLearningFxTradeData fsd'' tdl' tdlt' False False
  if not fail && (not $ null tdlts) 
    then return (True, True, tdl', tdlt', fsd'')
    else learningLoop 0 cl ce fsd

tradeLearningThread :: Fsd.FxSettingData ->
                       IO (Fsd.FxSettingData)
tradeLearningThread fsd = do
  printf "%s : learning\n" =<< Ftm.getLogTime
  e <- Fm.getOneChart Fm.getEndChartFromDB 
  (plsf, lsf, tdl, tdlt, fsd') <- learning False (Fcd.date e) fsd
  Fp.printLearningFxTradeData fsd' tdl tdlt plsf lsf
  printf "%s : learning done\n" =<< Ftm.getLogTime
  Fm.writeFxSettingData =<< Fm.readFxSettingData False fsd'
  
backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop retry fail n endN td fsd  = do
  (plsf, lsf, tdl, tdlt, fsd1) <- learning fail n fsd
  let ltt = Fs.getLearningTestTime fsd1
      sc = (foldl (\acc x -> acc + Ftd.trSuccess x) 0 tdlt)
  ctl <- Fm.getChartListBack    n (Fs.getPrepareTimeAll fsd1) 0
  ctt <- Fm.getChartListForward n (ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd) 0
  let ct = ctl ++ ctt
      (tdt, ctdl) = Ft.backTest (ltt * Gsd.learningTestCount Gsd.gsd) (Gsd.maxTradePeriod Gsd.gsd) sc td fsd1 ct
      n' = Fcd.date $ Ftd.chart tdt
      tdt' = Ft.resetFxalgorithmListCount tdt
  Fp.printTestProgress (retry && Ftd.profit tdt < Ftd.profit td) n n' fsd1 tdt tdl tdlt plsf lsf
  fsd2 <- Fm.writeFxSettingData =<< (Fm.readFxSettingData False $ Fs.updateFxSettingData ctdl td tdt fsd1)
  if retry && Ftd.profit tdt < Ftd.profit td
    then backTestLoop retry True n endN td fsd2
    else if endN <= n' || Ftd.realizedPL tdt' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
         then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt' , fsd2)
         else backTestLoop retry False n' endN tdt' fsd2
         
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
  waitTradeLoop =<< Fm.getOneChart Fm.getEndChartFromDB 

waitTradeLoop :: Fcd.FxChartData -> IO ()
waitTradeLoop p = do
  e <- Fm.getOneChart Fm.getEndChartFromDB 
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
  e <- Fm.getOneChart Fm.getEndChartFromDB 
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
  e <- Fm.getOneChart Fm.getEndChartFromDB 
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
    
    


