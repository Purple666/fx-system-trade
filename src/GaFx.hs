module GaFx
  ( backTest
  , trade
  , statistics
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Random
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

learningLoop :: Int ->
                Double ->
                Ga.LearningData  Fsd.FxSettingData ->
                IO (Int, Bool, [Ftd.FxTradeData], Fsd.FxSettingData)
learningLoop c pp fsl = do
  fsl' <- Ga.learning fsl
  let (p', tdltm, fsd'') = maximum .
                           map (\fsd' -> let tdlt = Ft.learning fsd'
                                             p    = Ftd.getEvaluationValueList tdlt
                                         in (p, tdlt, fsd')) $ Ga.getGaDataList fsl'
  -- Fp.printLearningFxTradeData p' 0 fsd'' tdltm 0 (Ft.evaluationOk tdl0 < profitstm) (pp == p')
  if Ft.evaluationOk tdltm
    then return (0, True, tdltm, fsd'')
    else if Fsd.getLearningTestTimes fsd'' < fromIntegral c || pp == p'
         then return (0, False, tdltm, Fsd.plusLearningTestTimes fsd'')
         else learningLoop (c + 1) p' fsl'

learning :: Int ->
            Fsd.FxSettingData ->
            IO (Int, Bool, [Ftd.FxTradeData], Fsd.FxSettingData)
learning n fsd = do
  let fsl = if M.member (Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd)
            then Fsd.fxSettingLog fsd
            else M.insert (Fsd.fxSetting fsd) (1, 1) $ Fsd.fxSettingLog fsd
  fsdm' <- M.fromList <$>
           (sequence $
            map (\(x, a) -> do let fsd' = fsd { Fsd.fxSetting = x }
                                   ltt  = Ta.getLearningTestTime fsd'
                               fc <- mapM (\_ -> do n' <- getRandomR(n - ltt * (Gsd.learningTestCount Gsd.gsd + Fsd.getLearningTestTimes fsd'), n)
                                                    cl <- Fm.getChartListBack n' (Ta.getPrepareTimeAll fsd' + ltt)
                                                    return (Fsd.FxChart { Fsd.chart = cl
                                                                        , Fsd.chartLength = ltt
                                                                        }))
                                     [1 .. Gsd.learningTestCount Gsd.gsd]
                               return (Fsd.nextFxSettingData fc fsd', a)) $ M.toList fsl)
  let r = M.filter (\(p, _, _, _) -> 0 < p) $
          M.mapWithKey (\fsd' (p, c) -> let tdlt = Ft.learning fsd'
                                            p'   = Ftd.getEvaluationValueList tdlt * (p + 1)
                                        in {- traceShow(p', Ftd.getEvaluationValueList tdlt, Ft.evaluationOk tdlt) $ -} (p', Ft.evaluationOk tdlt, tdlt, fsd')) $ M.filter (\(p, _) -> 0 < p) fsdm'
      tdlts = M.elems $ M.filter (\(_, y, _, _) -> y) r
      (_, _, tdlt', fsd'') = maximum tdlts
  if (not $ null tdlts)
    then return (length tdlts, True, tdlt',  fsd'')
    else if null r
         then learningLoop 0 0 . Ga.learningDataList . map Ga.learningData $ M.keys fsdm'
         else learningLoop 0 0 . Ga.learningDataList .
              map (\(_, _, _, fsd4) -> Ga.learningData fsd4) $ M.elems r

tradeLearning :: IO (Int, Fsd.FxSettingData)
tradeLearning = do
  fsd <- Fm.readFxSettingData "backtest"
  e <- Fm.getOneChart Fm.getEndChartFromDB
  (plsf, lsf, tdlt, fsd') <- learning (Fcd.no e) fsd
  Fp.printLearningFxTradeData 0 (Fcd.no e) fsd' tdlt plsf lsf
  return (Fcd.no e, fsd')

backTestLoop :: Bool ->
                Bool ->
                Int ->
                Int ->
                Ftd.FxTradeData ->
                Fsd.FxSettingData ->
                IO (Ftd.FxTradeData, Fsd.FxSettingData)
backTestLoop retry lf n endN td fsd = do
  (plsf, lok, tdlt, fsd1) <- if Ftd.side td == Ftd.None || lf
                             then learning n fsd
                             else return (0, True, [Ftd.initFxTradeDataCommon], fsd)
  let ltt = Ta.getLearningTestTime fsd1
  (fsd2, tdt) <- Ft.backTest ltt td fsd1
                 <$> ((++) <$>
                       Fm.getChartListBack    n (Ta.getPrepareTimeAll fsd1) <*>
                       Fm.getChartListForward n ltt)
  fsd3 <- Fm.writeFxSettingData "backtest"
          <$> Fs.updateFxSettingLog (Ftd.profit tdt - Ftd.profit td) fsd2
          =<< Fm.readFxSettingData "backtest"
  if Ftd.unrealizedPL tdt < Ftd.unrealizedPL td && Ftd.realizedPL tdt < Ftd.realizedPL td && retry
    then do Fp.printTestProgress fsd1 fsd td tdt tdlt plsf lok True
            backTestLoop retry True n endN td fsd3
    else do Fp.printTestProgress fsd1 fsd td tdt tdlt plsf lok False
            let n' = Fcd.no (Ftd.chart tdt) + 1
            if endN <= n' || Ftd.realizedPL tdt < Gsd.initalProperty Gsd.gsd / Gsd.quantityRate Gsd.gsd
              then return (tdt, fsd3)
              else backTestLoop retry (Ftd.unrealizedPL tdt <= Ftd.unrealizedPL td && Ftd.realizedPL tdt <= Ftd.realizedPL td) n' endN tdt fsd3

tradeEvaluate :: Ftd.FxTradeData ->
                 Fsd.FxSettingData ->
                 String ->
                 [Fcd.FxChartData] ->
                 IO (Ftd.FxTradeData)
tradeEvaluate td fsd coName xcd = do
  let (open, close, td1) = Ft.trade td fsd xcd
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
                         then do td1 <- tradeEvaluate td fsd1 coName =<<
                                        ((++) <$> Fm.getChartListBack (Fcd.no e - 1) (Ta.getPrepareTimeAll fsd1) <*> pure [e])
                                 -- Fp.printProgressFxTradeData td1 e                                 
                                 return (0, td1)
                   else return (sleep + 1, td)
  if 240 < sleep'
    then do return td2
    else tradeLoop e pl' sleep' td2 fsd1 coName

