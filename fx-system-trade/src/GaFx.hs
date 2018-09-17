module GaFx
  ( backTest
  , trade
  , debug
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Random
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
  reset             = Fs.resetFxSettingData
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
  fsd <- Fm.readFxSettingData Fsd.initFxSettingData
  debugLoop td fsd -- =<< async ()
  return ()

debugLoop :: Ftd.FxTradeData ->
             Fsd.FxSettingData ->
             IO Ftd.FxTradeData
debugLoop td fsd = do
  fsd' <- tradeLearningThread fsd
  e <- Fm.getOneChart Fm.getEndChartFromDB
  ct <- (++) <$> (init <$> Fm.getChartListBack (Fcd.no e) (Fs.getPrepareTimeAll fsd' + 1) 0) <*> pure [e]
  let (_, _, td') = Ft.trade td fsd' ct
  Fp.printStartTrade td'
  debugLoop td' fsd'

backTest :: Bool -> Int -> Int -> Bool -> IO ()
backTest retry s f latest = do
  fsd <- Fs.initFxsettingFromLog <$> (Fm.readFxSettingData Fsd.initFxSettingData)
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
  (fs, fsd') <- backTestLoop latest retry False n endN fsd td
  (s', f') <- if fs
              then do Fp.printBackTestResult "=================================" (s + 1) f fsd'
                      return (s + 1, f)
              else do Fp.printBackTestResult "---------------------------------" s (f + 1) fsd'
                      return (s, f + 1)
  backTest retry s' f' latest

trade :: Ftd.FxEnvironment -> String -> IO ()
trade environment coName = do
  c <- Fm.getOneChart Fm.getEndChartFromDB
  td <- Foa.updateFxTradeData =<< (Fm.updateFxTradeData coName $ (Ft.initFxTradeData environment) { Ftd.chart = c })
  Fp.printStartTrade td
  fsd <- tradeLearning =<< (Fs.initFxsettingFromLog <$> Fm.readFxSettingData Fsd.initFxSettingData)
  tradeWeeklyLoop fsd td coName

learningLoop :: Int ->
                [Fcd.FxChartData] ->
                [[Fcd.FxChartData]] ->
                Fsd.FxSettingData ->
                [Fsd.FxSettingData] ->
                IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c cl ce fsd fsds = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  fsds' <- map (\x -> let tdlt = map (\y -> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                            Fsd.nextFxSettingData ltt y x) ce
                          tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl x
                          p = if Ftd.getEvaluationValue tdl < 0 && Ftd.getEvaluationValueList tdlt < 0
                              then - (Ftd.getEvaluationValue tdl * Ftd.getEvaluationValueList tdlt)
                              else Ftd.getEvaluationValue tdl * Ftd.getEvaluationValueList tdlt
                      in (p, tdl, tdlt, x)) . (fsd:) . Ga.getGaDataList <$>
           Ga.learning (Fsd.nextFxSettingData lt cl fsd) (map (Fsd.nextFxSettingData lt cl) fsds)
  let (_, tdl, tdlt, fsd') = maximum fsds'
  --Fp.printLearningFxTradeData p 0 fsd' tdl tdlt 0 (Gsf.evaluationOk tdl tdlt) (fsd == fsd')
  if Ft.evaluationOk tdl tdlt
    then return (0, True, tdl, tdlt, fsd')
    else if fsd == fsd' && 0 < Ft.getProfitList tdlt
         then return (0, False, tdl, tdlt, Fsd.plusLearningTestTimes fsd')
         else if Fs.getLearningTestTimes fsd' < fromIntegral c
              then return (0, False, tdl, tdlt, Fsd.plusLearningTestTimes fsd')
              else learningLoop (c + 1) cl ce fsd' $ map (\(_, _, _, x) -> x) fsds'

learning :: Bool ->
            Int ->
            Fsd.FxSettingData ->
            IO (Int, Bool, Ftd.FxTradeData, [Ftd.FxTradeData], Fsd.FxSettingData)
learning failp n fsd = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  cl <-              Fm.getChartListBack n (Fs.getPrepareTimeAll fsd + lt) 0
  ce <- mapM ((\x -> Fm.getChartListBack (n - x) (Fs.getPrepareTimeAll fsd + ltt) 0) . (ltt *)) [0..Gsd.learningTestCount Gsd.gsd - 1]
  let tdlts = M.elems .
              M.filter (\(x, y, _, _, _) -> 0 < x && y) .
              M.mapWithKey (\y (p, c) -> let fsd' = fsd { Fsd.fxSetting = y }
                                             tdlt = map (\x-> Ft.learning (Ft.initFxTradeData Ftd.Backtest) $
                                                              Fsd.nextFxSettingData ltt x fsd') ce
                                             tdl  = Ft.learning (Ft.initFxTradeData Ftd.Backtest) $ Fsd.nextFxSettingData lt cl fsd'
                                         in ((Ftd.getEvaluationValue tdl + Ftd.getEvaluationValueList tdlt) *
                                             (p / fromIntegral c), Ft.evaluationOk tdl tdlt, tdl, tdlt, fsd')) .
              M.insert (Fsd.fxSetting fsd) (1, 1) $ Fsd.fxSettingLog fsd
      (_, _, tdl', tdlt', fsd'') = maximum tdlts
  if not $ null tdlts
    then return (length tdlts, True, tdl', tdlt', fsd'')
    else learningLoop 0 cl ce fsd . map (\x -> fsd { Fsd.fxSetting = x }) . M.keys $ Fsd.fxSettingLog fsd

tradeLearning :: Fsd.FxSettingData -> IO Fsd.FxSettingData
tradeLearning fsd = do
  e <- Fm.getOneChart Fm.getEndChartFromDB
  (plsf, lsf, tdl, tdlt, fsd') <- learning False (Fcd.no e) =<< Fm.readFxSettingData fsd
  Fp.printLearningFxTradeData 0 (Fcd.no e) fsd' tdl tdlt plsf lsf (fsd == fsd')
  return $ Fs.emptyFxSettingLog fsd'

tradeLearningThread :: Fsd.FxSettingData -> IO Fsd.FxSettingData
tradeLearningThread fsd = do
  threadDelay (5 * 60 * 1000 * 1000)
  tradeLearning fsd

backTestLoop :: Bool ->
                Bool ->
                Bool ->
                Int ->
                Int ->
                Fsd.FxSettingData ->
                Ftd.FxTradeData ->
                IO (Bool, Fsd.FxSettingData)
backTestLoop latest retry failp n endN fsd td = do
  (plsf, lsf, tdl, tdlt, fsd1) <- learning failp (n - 1) fsd
  let lt  = Fs.getLearningTime     fsd1
      ltt = Fs.getLearningTestTime fsd1
  (tdt, fsd2) <- Ft.backTest latest endN (lt + ltt * Gsd.learningTestCount Gsd.gsd)
                 (Ftd.trSuccess tdl + (sum $ map Ftd.trSuccess tdlt)) td fsd1
                        =<< ((++) <$>
                             Fm.getChartListBack    (n - 1) (Fs.getPrepareTimeAll fsd1) 0 <*>
                             Fm.getChartListForward n       (lt + ltt * Gsd.learningTestCount Gsd.gsd) 0)
  let n' = Fcd.no (Ftd.chart tdt) + 1
  Fp.printTestProgress (retry && Ftd.profit tdt < Ftd.profit td) (Fcd.date $ Ftd.chart td) (Fcd.date $ Ftd.chart tdt) fsd2 tdt tdl tdlt plsf lsf
  if retry && Ftd.profit tdt < Ftd.profit td
    then do let fsd3 = Fs.deleteFxsettingFromLog fsd2
            fsd4 <- Fs.resetFxSettingData fsd3
            backTestLoop latest retry True n endN fsd4 td
    else if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
         then return (Gsd.initalProperty Gsd.gsd < Ftd.realizedPL tdt, fsd2)
         else backTestLoop latest retry False n' endN fsd2 tdt

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

tradeWeeklyLoop :: Fsd.FxSettingData ->
                   Ftd.FxTradeData ->
                   String ->
                   IO ()
tradeWeeklyLoop fsd td coName = do
  waitTrade
  fsd' <- tradeLearning fsd
  e <- Foa.getNowPrices td
  td' <- tradeLoop e 0 td fsd' coName =<< async (tradeLearningThread fsd')
  tdw <- Fm.updateFxTradeData (coName ++ "_weekly") td
  Ftw.tweetWeek tdw td'
  Fm.setFxTradeData (coName ++ "_weekly") td'
  tradeWeeklyLoop fsd td' coName

checkTradeLearning :: Async Fsd.FxSettingData ->
                      Fsd.FxSettingData ->
                      IO (Async Fsd.FxSettingData, Fsd.FxSettingData)
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
             String ->
             Async Fsd.FxSettingData ->
             IO Ftd.FxTradeData
tradeLoop p sleep td fsd coName a = do
  (a', fsd') <- checkTradeLearning a fsd
  e <- Foa.getNowPrices td
  (sleep', td2) <- if e /= p
                   then do td1 <- tradeEvaluate td fsd' coName =<< ((++) <$> Fm.getChartListBack (Fcd.no e - 1) (Fs.getPrepareTimeAll fsd') 0 <*> pure [e])
                           return (0, td1)
                   else return (sleep + 1, td)
  t <- getCurrentTime
  threadDelay ((15 - (truncate (utcTimeToPOSIXSeconds t) `mod` 15)) * 1000 * 1000)
  if 240 < sleep'
    then do cancel a'
            return td2
    else tradeLoop e sleep' td2 fsd' coName a'
