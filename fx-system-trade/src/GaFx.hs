module GaFx
  ( backTest
  , trade
  , debug
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Random
import           Control.DeepSeq
import qualified Data.Map                 as M
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
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

instance Ga.Ga Fsd.FxSettingData where
  copy              = Fs.copyFxSettingData
  mutation          = Fs.mutationFxSettingData
  crossover         = Fs.crossoverFxSettingData
  createInitialData = Fs.createInitialGaData
  learningEvaluate  = Ft.gaLearningEvaluate
  getGaLoopMax      = Fsd.getLearningTestTimes
  plusGaLoopMax     = Fsd.plusLearningTestTimes

debug :: IO ()
debug = do
  let td  = Ft.initFxTradeData Ftd.Backtest
  fsd <- Fm.readFxSettingData "debug"
  debugLoop td fsd -- =<< async ()
  return ()

debugLoop :: Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             IO Ftd.FxTradeData
debugLoop td fsd = do
  e <- Fm.getOneChart Fm.getEndChartFromDB
  ct <- (++) <$> (init <$> Fm.getChartListBack (Fcd.no e) (Fs.getPrepareTimeAll fsd + 1) 0) <*> pure [e]
  let (_, _, td') = Ft.trade td fsd ct
  debugLoop td' fsd

backTest :: Int -> Int -> Bool -> IO ()
backTest s f latest = do
  fsd <- Fm.readFxSettingData "backtest"
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      p = Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd
  endN <- Fcd.no <$> Fm.getOneChart Fm.getEndChartFromDB
  startN <- if latest
            then return (endN - (p + 24 * 60 * 10 + ltt * Gsd.learningTestCount Gsd.gsd))
            else do s <- Fcd.no <$> Fm.getOneChart Fm.getStartChartFromDB
                    getRandomR(s, s + ltt * 2)
  let n = startN + p
  (fs, fsd') <- backTestLoop latest n endN td fsd
  (s', f') <- if fs
              then do Fp.printBackTestResult "=================================" (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" s (f + 1) fsd'
                      return (s, f + 1)
  backTest s' f' latest

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB
  td <- Foa.updateFxTradeData =<< (Fm.updateFxTradeData coName $ (Ft.initFxTradeData environment) { Ftd.chart = c })
  Fp.printProgressFxTradeData td c
  tradeWeeklyLoop td coName =<< Fm.readFxSettingData "backtest"

learningLoop :: Int ->
                Int ->
                Fsd.FxSettingData ->
                IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c n fsd = do
  let lt   = Fs.getLearningTime     fsd
      ltt  = Fs.getLearningTestTime fsd
  cl <-              Fm.getChartListBack n (Fs.getPrepareTimeAll fsd + lt) 0
  ce <- mapM ((\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd + ltt) 0) .
              (ltt *)) [0..Gsd.learningTestCount Gsd.gsd - 1]
  fsds' <- map (\x -> let tdlt = map (\y -> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                            Fsd.nextFxSettingData ltt y x) ce
                          tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl x
                          p    = Ftd.getEvaluationValue tdl + Ftd.getEvaluationValueList tdlt
                      in (p, tdl, tdlt, x)) . Ga.getGaDataList <$>
           (Ga.learning . Ga.learningData $ Fsd.nextFxSettingData lt cl fsd)
  let (p, tdl, tdlt, fsd') = maximum fsds'
   Fp.printLearningFxTradeData p 0 lt ltt fsd' tdl tdlt 0 (Ft.evaluationOk tdl tdlt) (fsd == fsd')
  if Ft.evaluationOk tdl tdlt
    then return (0, True, tdl, tdlt, fsd')
    else if Fs.getLearningTestTimes fsd' < fromIntegral c || fsd == fsd' -- && Ft.evaluationOk2 tdl tdlt)
         then return (0, False, tdl, tdlt, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) n fsd' 

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  tdlts <- (M.elems .
            M.filter (\(x, y, _, _, _) -> 0 < x && y)) <$>
           (sequence . M.mapWithKey (\y (p, c) -> do let fsd' = fsd { Fsd.fxSetting = y }
                                                         lt   = Fs.getLearningTime     fsd'
                                                         ltt  = Fs.getLearningTestTime fsd'
                                                     cl <-              Fm.getChartListBack n (Fs.getPrepareTimeAll fsd' + lt) 0
                                                     ce <- mapM ((\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd' + ltt) 0) .
                                                                 (ltt *)) [0..Gsd.learningTestCount Gsd.gsd - 1]
                                                     let tdlt = map (\x-> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                                          Fsd.nextFxSettingData ltt x fsd') ce
                                                         tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                                     return (10000 * (Ftd.getEvaluationValue tdl + Ftd.getEvaluationValueList tdlt) *
                                                             (p / fromIntegral c) / fromIntegral (lt + ltt * Gsd.learningTestCount Gsd.gsd),
                                                             Ft.evaluationOk tdl tdlt, tdl, tdlt, fsd')) .
            M.insert (Fsd.fxSetting fsd) (10000, 1) $ Fsd.fxSettingLog fsd)
  let (_, _, tdl', tdlt', fsd'') = maximum tdlts
  if not $ null tdlts
    then return (length tdlts, True, tdl', tdlt',  fsd'')
    else learningLoop 0 n fsd

tradeLearning :: IO Fsd.FxSettingData
tradeLearning = do
  e <- Fm.getOneChart Fm.getEndChartFromDB 
  (plsf, lsf, tdl, tdlt, fsd') <- learning (Fcd.no e) =<< Fm.readFxSettingData "backtest"
  -- Fp.printLearningFxTradeData 0 (Fcd.no e) fsd' tdl tdlt plsf lsf
  return fsd'

tradeLearningThread :: IO Fsd.FxSettingData
tradeLearningThread = do
  -- threadDelay (5 * 60 * 1000 * 1000)
  tradeLearning

backTestLoop :: Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop latest n endN td fsd = do
  (plsf, lsf, tdl, tdlt, fsd1) <- learning n fsd
  (fsd2, tdt) <- let lt  = Fs.getLearningTime     fsd1
                     ltt = Fs.getLearningTestTime fsd1
                 in Ft.backTest latest (lt + ltt * Gsd.learningTestCount Gsd.gsd) plsf td fsd1
                    =<< ((++) <$>
                          Fm.getChartListBack    (n - 1) (Fs.getPrepareTimeAll fsd1) 0 <*>
                          Fm.getChartListForward n       (lt + ltt * Gsd.learningTestCount Gsd.gsd) 0)
  let n' = Fcd.no (Ftd.chart tdt) + 1
  Fp.printTestProgress (Fcd.date $ Ftd.chart td) (Fcd.date $ Ftd.chart tdt) fsd1 fsd tdt tdl tdlt plsf lsf
  if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
    then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt, fsd2)
    else backTestLoop latest n' endN tdt fsd2

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 String ->
                 [Fcd.FxChartData] ->
                 IO Ftd.FxTradeData
tradeEvaluate td fsd coName xcd = do
  let (open, close, td1) = Ft.trade td fsd xcd
  td3 <- if close /= Ftd.None
         then do td2 <- Foa.close td1
                 Fm.setFxTradeData coName td2
                 Fp.printTradeResult open close td td2 0
                 return td2
         else return td1
  if open /= Ftd.None
  then do (units, td4) <- Foa.open td3 open
          Fm.setFxTradeData coName td4
          Fp.printTradeResult open close td td4 units
          return td4
  else return td3

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
                   Fsd.FxSettingData ->
                   IO ()
tradeWeeklyLoop td coName fsd = do
  waitTrade
  fsd' <- tradeLearning
  e <- Foa.getNowPrices td
  td' <- tradeLoop e 0 td fsd' coName =<< (async $ tradeLearningThread)
  tdw <- Fm.updateFxTradeData (coName ++ "_weekly") td
  Ftw.tweetWeek tdw td'
  Fm.setFxTradeData (coName ++ "_weekly") td'
  tradeWeeklyLoop td' coName fsd'

checkTradeLearning :: Async Fsd.FxSettingData ->
                      Fsd.FxSettingData ->
                      IO (Async Fsd.FxSettingData, Fsd.FxSettingData)
checkTradeLearning a fsd = do
  e <- poll a
  case e of
    Nothing -> return (a, fsd)
    Just _  -> do fsd' <- wait a
                  a' <- async $ tradeLearningThread
                  return (a', fsd')

tradeLoop :: Fcd.FxChartData ->
             Int ->
             Ftd.FxTradeData ->
             Fsd.FxSettingData ->            
             String ->
             Async Fsd.FxSettingData ->
             IO Ftd.FxTradeData
tradeLoop p sleep td fsd coName a = do
  -- t <- getCurrentTime
  -- threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  -- (a', fsd') <- return (a, fsd)
  e <- Foa.getNowPrices td
  (sleep', td2, a2, fsd2) <- if (Fcd.close e) /= (Fcd.close p)
                             then do (a1, fsd1) <- checkTradeLearning a fsd
                                     td1 <- tradeEvaluate td fsd1 coName =<<
                                            ((++) <$> Fm.getChartListBack (Fcd.no e - 1) (Fs.getPrepareTimeAll fsd1) 0 <*> pure [e])
                                     -- Fp.printProgressFxTradeData td1 e                                 
                                     return (0, td1, a1, fsd1)
                             else return (sleep + 1, td, a, fsd)
  if 3600 < sleep'
    then do cancel a2
            return td2
    else tradeLoop e sleep' td2 fsd2 coName a2

