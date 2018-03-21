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
  backTestMainLoop retry startN 0 0 
  
backTestMainLoop :: Bool -> Int -> Int -> Int -> IO ()
backTestMainLoop retry start s f = do
  startN <- Fcd.date <$> Fm.getOneChart Fm.getStartChartFromDB
  endN <- Fcd.date <$> Fm.getOneChart Fm.getEndChartFromDB
  fsd <- Fm.readFxSettingData
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      n   = start + Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd
  fs <- backTestLoop retry False n endN td
  (s', f') <- if fs
              then do printf "================================= %d - %d \n" (s + 1) f 
                      return (s + 1, f)
              else do printf "--------------------------------- %d - %d \n" s (f + 1)
                      return (s, f + 1)
  start' <- getRandomR(startN, startN + ltt * 2)
  backTestMainLoop retry start' s' f' 

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB 
  td <- Fm.updateFxTradeData coName =<< (Foa.updateFxTradeData $ Ft.initFxTradeData environment)
  let td' = td { Ftd.chart = c }
  Fp.printStartTrade td'
  tradeWeeklyLoop td' 

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
              M.filter (\(_, y, _, _, _) -> y) .
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

tradeLearningThread :: IO (Fsd.FxSettingData)
tradeLearningThread  = do
  printf "%s : learning\n" =<< Ftm.getLogTime
  e <- Fm.getOneChart Fm.getEndChartFromDB 
  (plsf, lsf, tdl, tdlt, fsd) <- learning False (Fcd.date e) =<< Fm.readFxSettingData
  Fp.printLearningFxTradeData fsd tdl tdlt plsf lsf
  printf "%s : learning done\n" =<< Ftm.getLogTime
  return fsd
  
backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                IO (Bool)
backTestLoop retry fail n endN td = do
  (plsf, lsf, tdl, tdlt, fsd) <- learning fail n =<< Fm.readFxSettingData
  let ltt = Fs.getLearningTestTime fsd
      sc = (foldl (\acc x -> acc + Ftd.trSuccess x) 0 tdlt)
  ctl <- Fm.getChartListBack    n (Fs.getPrepareTimeAll fsd) 0
  ctt <- Fm.getChartListForward n (ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd) 0
  let ct = ctl ++ ctt
      (tdt, fsd') = Ft.backTest (ltt * Gsd.learningTestCount Gsd.gsd) (Gsd.maxTradePeriod Gsd.gsd) sc td fsd ct
      n' = Fcd.date $ Ftd.chart tdt
      tdt' = Ft.resetFxalgorithmListCount tdt
  Fp.printTestProgress (retry && Ftd.profit tdt < Ftd.profit td) n n' fsd' tdt tdl tdlt plsf lsf
  if retry
    then Fm.writeFxSettingData fsd'
    else return ()
  if retry && Ftd.profit tdt < Ftd.profit td
    then backTestLoop retry True n endN td 
    else if endN <= n' || Ftd.realizedPL tdt' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
         then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt')
         else backTestLoop retry False n' endN tdt'
         
tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 [Fcd.FxChartData] ->
                 IO (Ftd.FxTradeData)
tradeEvaluate td fsd xcd = do
  let (open, close, td1) = Ft.trade td fsd xcd
  td3 <- if close /= Ftd.None
         then do td2 <- Foa.close td1
                 Fm.setFxTradeData "trade_practice" td2
                 Fp.printTradeResult td td2 0
                 return td2
         else return td1
  td5 <- if open /= Ftd.None
         then do (units, td4) <- Foa.open td3 open
                 Fm.setFxTradeData "trade_practice" td4
                 Fp.printTradeResult td td4 units
                 return td4
         else return td3
  return td5

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
                   IO ()
tradeWeeklyLoop td = do
  waitTrade
  e <- Fm.getOneChart Fm.getEndChartFromDB
  fsd <- tradeLearningThread 
  td' <- tradeLoop e 0 0 td fsd =<< async (tradeLearningThread)
  tds <- Fm.updateFxTradeData "trade_practice_weekly" td'
  Ftw.tweetWeek tds td'
  Fm.setFxTradeData "trade_practice_weekly" td'
  tradeWeeklyLoop td' 

tradeLearning :: Async (Fsd.FxSettingData) ->
                 Fsd.FxSettingData ->
                 IO (Async (Fsd.FxSettingData), Fsd.FxSettingData)
tradeLearning a fsd = do
  e <- poll a
  case e of
    Nothing -> return (a, fsd)
    Just _  -> do fsd' <- wait a
                  a' <- async (tradeLearningThread)
                  return (a', fsd')
                 
tradeLoop :: Fcd.FxChartData ->
             Int ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             Async (Fsd.FxSettingData) ->
             IO (Ftd.FxTradeData)
tradeLoop p sleep lc td fsd a = do
  (a', fsd1, lc') <- if 60 < lc
                    then do (a', fsd') <- tradeLearning a fsd
                            return (a', fsd', 0)
                    else return (a, fsd, lc + 1)
  e <- Fm.getOneChart Fm.getEndChartFromDB 
  ct <- Fm.getChartListBack (Fcd.date e) (Fs.getPrepareTimeAll fsd1 + 1) 0
  (sleep', td2) <- if last ct /= p
                   then do td1 <- tradeEvaluate td fsd1 ct
                           return (0, td1)
                   else return (sleep + 1, td)
  t <- getCurrentTime
  threadDelay ((60 - ((truncate $ utcTimeToPOSIXSeconds t) `mod` 60)) * 1000 * 1000)
  if 30 < sleep' 
    then do _ <- wait a'
            return td2
    else tradeLoop (last ct) sleep' lc'  td2 fsd1 a'
    

