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
  reset             = Fs.resetFxSettingData
  setHash           = Fs.setHashFxSettingData

debug :: IO ()
debug = do
  fsd <- Fm.readFxSettingData "backtest"
  traceShow(M.elems $ Fsd.fxSettingLog fsd) $ return ()
  return ()

backTest :: String -> Bool -> Bool -> IO ()
backTest coName latest retry = do
  fsd <- Fm.readFxSettingData "backtest"
  (s, f) <- Fm.readResult coName
  let td  = Ft.initFxTradeData Ftd.Backtest
      ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      p = Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd
  endN <- Fcd.no <$> Fm.getOneChart Fm.getEndChartFromDB
  sn <- if latest
        then return (endN - (p + ltt * Gsd.learningTestCount Gsd.gsd + Gsd.backtestLatestTime Gsd.gsd))
        else Fcd.no <$> Fm.getOneChart Fm.getStartChartFromDB
  startN <- (+) <$> getRandomR(sn, sn + ltt * 2) <*> pure p
  let n = startN
  (fs, fsd') <- backTestLoop retry False 0 n startN endN td fsd
  (s', f') <- if fs
              then do Fp.printBackTestResult "=================================" (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" s (f + 1) fsd'
                      return (s, f + 1)
  Fm.writeResult coName s' f'
  backTest coName latest retry

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB
  td <- Foa.updateFxTradeData =<< (Fm.updateFxTradeData coName $ (Ft.initFxTradeData environment) { Ftd.chart = c })
  Fp.printProgressFxTradeData td c
  tradeWeeklyLoop td coName

learningLoop :: Int ->
                Int ->
                Int ->
                Fsd.FxSettingData ->
                IO (Int, Bool, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c n startN fsd = do
  let lt = Fs.getLearningTime fsd
  n' <- getRandomR(startN, n)
  cl <- Fm.getChartListBack n' (Fs.getPrepareTimeAll fsd + lt) 0
  r <- (Ga.learning . Ga.learningData $ Fsd.nextFxSettingData lt cl fsd)
  (p', tdlt, fsd') <- maximum <$>
                      (sequence $
                       (map (\x -> do let ltt' = Fs.getLearningTestTime x
                                      ce' <- sequence $ map (\_ -> do n' <- getRandomR(startN, n)
                                                                      Fm.getChartListBack n' (Fs.getPrepareTimeAll x + ltt') 0) 
                                             [0..Gsd.learningTestCount Gsd.gsd - 1]
                                      let tdlt = map (\y -> Ft.learning $ Fsd.nextFxSettingData ltt' y x) ce'
                                          p    = Ftd.getEvaluationValueList tdlt
                                      return (p, tdlt, x)) . (fsd:) $ Ga.getGaDataList r))
  -- Fp.printLearningFxTradeData p' 0 fsd' tdl tdlt 0 (Ft.evaluationOk tdl tdlt) (fsd' == fsd)
  if Ft.evaluationOk tdlt
    then return (0, True, tdlt, fsd')
    else if (Fsd.learningTestTimes . Fsd.learningSetting $ Fsd.fxSetting fsd') < fromIntegral c || fsd' == fsd
         then return (0, False, tdlt, Fsd.plusLearningTestTimes fsd')
         else learningLoop (c + 1) n startN fsd' 

learning :: Int ->
            Int ->
            Fsd.FxSettingData ->
            IO (Int, Bool, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n startN fsd = do
{-
  
      lt  = maximum . M.elems $ M.mapWithKey (\x (_, _) -> Fs.getLearningTime     fsd { Fsd.fxSetting = x }) fsl
      ltt = maximum . M.elems $ M.mapWithKey (\x (_, _) -> Fs.getLearningTestTime fsd { Fsd.fxSetting = x }) fsl
      pre = maximum . M.elems $ M.mapWithKey (\x (_, _) -> Fs.getPrepareTimeAll   fsd { Fsd.fxSetting = x }) fsl
  xcd <- Fm.getChartListBack n ((pre + lt + ltt * Gsd.learningTestCount Gsd.gsd) * 2) 0
  let tdlts = M.elems .
              M.filter (\(y, _, _, _) -> y) $
              M.mapWithKey (\y ( p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                              lt'  = Fs.getLearningTime     fsd'
                                              ltt' = Fs.getLearningTestTime fsd'
                                              cl   = Fcd.getChartListBack 0 (Fs.getPrepareTimeAll fsd' + lt') xcd
                                              ce   =  map (\x -> Fcd.getChartListBack (ltt' * x + lt')
                                                            (Fs.getPrepareTimeAll fsd' + ltt') xcd)
                                                      [0..Gsd.learningTestCount Gsd.gsd - 1]
                                              tdlt = map (\x-> Ft.learning $ Fsd.nextFxSettingData ltt' x fsd') ce
                                              tdl  = Ft.learning $ Fsd.nextFxSettingData lt' cl fsd'
                                              p'   = (Ftd.getEvaluationValue tdl + Ftd.getEvaluationValueList tdlt) *
                                                     (p / fromIntegral c)
                                          in (Ft.evaluationOk tdl tdlt, tdl, tdlt, fsd')) fsl
-}
  let fsl = if M.member (Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd)
            then Fsd.fxSettingLog fsd
            else M.insert (Fsd.fxSetting fsd) (1, 1) $ Fsd.fxSettingLog fsd
  tdlts <- (M.elems . M.filter (\(_, y, _, _) -> y)) <$>
           (sequence $
            M.mapWithKey (\y (p, c) -> do let fsd' = fsd { Fsd.fxSetting = y }
                                              ltt' = Fs.getLearningTestTime fsd'
                                          ce  <- sequence $ map (\_ -> do n' <- getRandomR(startN, n)
                                                                          Fm.getChartListBack n' (Fs.getPrepareTimeAll fsd' + ltt') 0) 
                                                 [0..Gsd.learningTestCount Gsd.gsd - 1]
                                          let tdlt = map (\x-> Ft.learning $ Fsd.nextFxSettingData ltt' x fsd') ce
                                              p'   = Ftd.getEvaluationValueList tdlt * (p * fromIntegral c)
                                          return (p', Ft.evaluationOk tdlt, tdlt, fsd')) fsl)
  let (_, _, tdlt', fsd'') = maximum tdlts
  if not $ null tdlts
    then return (length tdlts, True, tdlt',  fsd'')
    else learningLoop 0 n startN fsd 

tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData "backtest"
  e <- Fm.getOneChart Fm.getEndChartFromDB
  let ltt = Fs.getLearningTestTime fsd
      lt  = Fs.getLearningTime fsd
      s = Fs.getPrepareTimeAll fsd + lt + ltt * Gsd.learningTestCount Gsd.gsd
  (plsf, lsf, tdlt, fsd') <- learning (Fcd.no e) s fsd
  -- Fp.printLearningFxTradeData 0 (Fcd.no e) fsd' tdl tdlt plsf lsf
  return (Fcd.no e, fsd')

backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop retry lf plsf n startN endN td fsd = do
  (plsf', lok, tdlt, fsd1) <- if Ftd.side td == Ftd.None || lf || (not $ M.member (Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd))
                             then learning n startN fsd
                             else return (plsf, False, [Ftd.initFxTradeDataCommon], fsd)
  let lt  = Fs.getLearningTime     fsd1
      ltt = Fs.getLearningTestTime fsd1
  (fsd2, tdt) <- Ft.backTest (0 < plsf') (lt + ltt * Gsd.learningTestCount Gsd.gsd) td fsd1
                 <$> ((++) <$>
                       Fm.getChartListBack    (n - 1) (Fs.getPrepareTimeAll fsd1) 0 <*>
                       Fm.getChartListForward n       (lt + ltt * Gsd.learningTestCount Gsd.gsd) 0)
  if Ftd.realizedPL tdt < Ftd.realizedPL td && retry && not lok && Ftd.side td == Ftd.None
    then do Fp.printTestProgress fsd1 fsd td tdt tdlt plsf' lok True
            backTestLoop retry True plsf' n startN endN td =<< (Ga.getHeadGaData <$> (Fs.resetFxSettingData $ Ga.learningData fsd))
    else do fsd3 <- Fm.writeFxSettingData "backtest"
                    <$> Fs.updateFxSettingLog plsf' ((Ftd.realizedPL tdt - Ftd.realizedPL td) / (fromIntegral $ (lt + ltt * Gsd.learningTestCount Gsd.gsd))) fsd2
                    =<< Fm.readFxSettingData "backtest"
            Fp.printTestProgress fsd1 fsd td tdt tdlt plsf' lok False
            let n' = Fcd.no (Ftd.chart tdt) + 1
            if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
              then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt, fsd2)
              else backTestLoop retry (Ftd.realizedPL tdt < Ftd.realizedPL td) plsf' n' startN endN tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 String ->
                 [Fcd.FxChartData] ->
                 IO (Fsd.FxSettingData, Ftd.FxTradeData)
tradeEvaluate td fsd coName xcd = do
  let (open, close, fsd1, td1) = Ft.trade td fsd xcd
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
  return (fsd1, td5)

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
  -- Ftw.tweetWeek tdw td'
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
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  e <- Foa.getNowPrices td
  (pl', fsd1) <- if Ftd.side td == Ftd.None && lt + ltt * Gsd.learningTestCount Gsd.gsd < Fcd.no e - pl
                 then tradeLearning
                 else return (pl, fsd)
  (sleep', td2, fsd3) <- if (Fcd.close e) /= (Fcd.close p)
                         then do (fsd2, td1) <- tradeEvaluate td fsd1 coName =<<
                                                ((++) <$> Fm.getChartListBack (Fcd.no e - 1) (Fs.getPrepareTimeAll fsd1) 0 <*> pure [e])
                                 -- Fp.printProgressFxTradeData td1 e                                 
                                 return (0, td1, fsd2)
                         else return (sleep + 1, td, fsd1)
  if 240 < sleep'
    then do return td2
    else tradeLoop e pl' sleep' td2 fsd3 coName

