module GaFx
  ( backTest
  , trade
  , debug
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

debug :: IO ()
debug = do
  debugLoop =<< (Fm.readFxSettingData $ Fsd.initFxSettingData)
  return ()

debugLoop :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
debugLoop fsd = do
  debugLoop =<< tradeLearning fsd

backTest :: Bool -> Int -> Int -> IO ()
backTest retry s f = do
  startN <- Fcd.date <$> Fm.getOneChart Fm.getStartChartFromDB
  endN <- Fcd.date <$> Fm.getOneChart Fm.getEndChartFromDB
  fsd <- Fm.readFxSettingData $ Fsd.initFxSettingData 
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
  start' <- getRandomR(startN, startN + ltt * 2)
  let n   = start' + Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd
  fs <- backTestLoop retry False n endN fsd td
  (s', f') <- if fs
              then do printf "================================= %d - %d \n" (s + 1) f 
                      return (s + 1, f)
              else do printf "--------------------------------- %d - %d \n" s (f + 1)
                      return (s, f + 1)
  backTest retry s' f' 

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB 
  td <- Fm.updateFxTradeData coName =<< (Foa.updateFxTradeData $ (Ft.initFxTradeData environment) { Ftd.chart = c }) 
  fsd <- Fm.readFxSettingData $ Fsd.initFxSettingData
  Fm.setFxTradeData "trade_practice_weekly" <$> Fm.updateFxTradeData "trade_practice_weekly" td
  Fp.printStartTrade td
  tradeWeeklyLoop fsd td

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
                     (Ga.learning $ Fsd.nextFxSettingData lt cl fsd)
  let tdl = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
  --Fp.printLearningFxTradeData fsd'' tdl tdlt
  if Gsf.evaluationOk tdl tdlt
    then return (0, True, tdl, tdlt, fsd')
    else if Fs.getLearningTestTimes fsd' < fromIntegral c
         then return (0, False, tdl, tdlt, Fsd.plusGaLength $ Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) cl ce fsd'
              
learning :: Bool ->
            Int ->
            Fsd.FxSettingData -> 
            IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning failp n fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  cl <-             Fm.getChartListBack (n - ltt * Gsd.learningTestCount Gsd.gsd) (Fs.getPrepareTimeAll fsd + lt) 0
  ce <- mapM (\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd + ltt) 0) $ map (ltt *) [0..Gsd.learningTestCount Gsd.gsd - 1]
  let tdlts = M.elems .
              M.filter (\(x, y, _, _, _) -> 0 < x && y) .
              M.mapWithKey (\y (p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                             tdlt = map (\x-> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                              Fsd.nextFxSettingData ltt x fsd') ce
                                             tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                         in (Gsf.getEvaluationValueList tdlt * (p / fromIntegral c), Gsf.evaluationOk tdl tdlt,
                                             tdl, tdlt, fsd')) .
              M.insert (Fsd.fxSetting fsd) (100000, 1) $ Fsd.fxSettingLog fsd
      (_, _, tdl', tdlt', fsd'') = maximum tdlts
  -- Fp.printLearningFxTradeData fsd'' tdl' tdlt' False False
  if not failp && (not $ null tdlts) 
    then return (length tdlts, True, tdl', tdlt', fsd'')
    else learningLoop 0 cl ce fsd

tradeLearning :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
tradeLearning fsd = do
  e <- Fm.getOneChart Fm.getEndChartFromDB 
  (plsf, lsf, tdl, tdlt, fsd') <- learning False (Fcd.date e) =<< Fm.readFxSettingData fsd
  Fp.printLearningFxTradeData (Fcd.date e) fsd' tdl tdlt plsf lsf (fsd == fsd')
  return fsd'
  
tradeLearningThread :: Fsd.FxSettingData -> IO (Fsd.FxSettingData)
tradeLearningThread  fsd = do
  threadDelay (60 * 1000 * 1000)
  tradeLearning fsd

backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Fsd.FxSettingData ->
                Ftd.FxTradeData ->
                IO (Bool)
backTestLoop retry failp n endN fsd td = do
  (plsf, lsf, tdl, tdlt, fsd1) <- learning failp n fsd
  let ltt = Fs.getLearningTestTime fsd1
  (tdt, fsd2) <- Ft.backTest (ltt * Gsd.learningTestCount Gsd.gsd) (Gsd.maxTradePeriod Gsd.gsd) (Ftd.trSuccess $ sum tdlt) td fsd1
                 =<< ((++) <$> 
                      Fm.getChartListBack    n (Fs.getPrepareTimeAll fsd1) 0 <*>
                      Fm.getChartListForward n (ltt * Gsd.learningTestCount Gsd.gsd + Gsd.maxTradePeriod Gsd.gsd) 0)
  let n' = Fcd.date $ Ftd.chart tdt
      tdt' = Ft.resetFxalgorithmListCount tdt
  Fp.printTestProgress (retry && Ftd.profit tdt < Ftd.profit td) n n' fsd2 tdt tdl tdlt plsf lsf
  if retry && Ftd.profit tdt < Ftd.profit td
    then backTestLoop retry True n endN fsd2 td 
    else if endN <= n' || Ftd.realizedPL tdt' < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
         then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt')
         else backTestLoop retry False n' endN fsd2 tdt'
         
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
    else do threadDelay (10 * 1000 * 1000)
            waitTradeLoop e

tradeWeeklyLoop :: Fsd.FxSettingData ->
                   Ftd.FxTradeData ->
                   IO ()
tradeWeeklyLoop fsd td = do
  waitTrade
  e <- Fm.getOneChart Fm.getEndChartFromDB
  fsd' <- tradeLearning fsd
  td' <- tradeLoop e 0 td fsd' =<< async (tradeLearningThread fsd')
  tds <- Fm.updateFxTradeData "trade_practice_weekly" td'
  Ftw.tweetWeek tds td'
  Fm.setFxTradeData "trade_practice_weekly" td'
  tradeWeeklyLoop fsd td' 

checkTradeLearning :: Async (Fsd.FxSettingData) ->
                      Fsd.FxSettingData ->
                      IO (Async (Fsd.FxSettingData), Fsd.FxSettingData)
checkTradeLearning a fsd = do
  e <- poll a
  case e of
    Nothing -> return (a, fsd)
    Just _  -> do fsd' <- wait a
                  a' <- async (tradeLearningThread fsd')
                  return (a', fsd')
                 
tradeLoop :: Fcd.FxChartData ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             Async (Fsd.FxSettingData) ->
             IO (Ftd.FxTradeData)
tradeLoop p sleep td fsd a = do
  (a', fsd') <- checkTradeLearning a fsd
  e <- Foa.getNowPrices td
  ct <- (++) <$> (init <$> Fm.getChartListBack (Fcd.date e) (Fs.getPrepareTimeAll fsd' + 1) 0) <*> pure [e]
  (sleep', td2) <- if last ct /= p
                   then do td1 <- tradeEvaluate td fsd' ct
                           return (0, td1)
                   else return (sleep + 1, td)
  t <- getCurrentTime
  threadDelay ((60 - ((truncate $ utcTimeToPOSIXSeconds t) `mod` 60)) * 1000 * 1000)
  if 30 < sleep' 
    then do cancel a'
            return td2
    else tradeLoop (last ct) sleep' td2 fsd' a'
    
