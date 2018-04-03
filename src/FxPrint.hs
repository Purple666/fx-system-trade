module FxPrint
  ( printStartTrade
  , printTradeResult
  , printTestProgress
  , printLearningFxTradeData
  ) where

--import Debug.Trace
import Text.Printf
import qualified FxChartData              as Fcd
import qualified FxSettingData            as Fsd
import qualified FxSetting                as Fs
import qualified FxTradeData              as Ftd
import qualified FxTime                   as Ftm

printTestProgress :: Bool -> Int -> Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int ->Bool -> IO ()
printTestProgress retry n n' fsd tdt tdl tdlt plsf lsf = do
-- let lt  = truncate $ (fromIntegral $ Fs.getLearningTime fsd) / (60 * 24 :: Double)     :: Int
--     ltt = truncate $ (fromIntegral $ Fs.getLearningTestTime fsd) / (60 * 24 :: Double) :: Int
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  if retry
    then printf "   "
    else return ()
  printf "%s : " =<< Ftm.getLogTime
  printf "%s-%s : %6d %6d %6.2f %4s "
    (Fcd.getDate n)
    (Fcd.getDate n')
    lt
    ltt
    (Fcd.close $ Ftd.chart tdt)
    (show (Ftd.side tdt))
  printFxTradeData tdt
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "| %3d %c %3d\n" plsf (head $ show lsf) (length $ Fsd.fxSettingLog fsd)

printLearningFxTradeData :: Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> Bool -> IO ()
printLearningFxTradeData n fsd tdl tdlt plsf lsf fs = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  printf "%s Learning " =<< Ftm.getLogTime
  printf "%8d %6d %6d " n lt ltt
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "| %3d %c %3d %c\n" plsf (head $ show lsf) (length $ Fsd.fxSettingLog fsd) (head $ show fs) 

printStartTrade :: Ftd.FxTradeData  -> IO ()
printStartTrade td = do
  printf "%s " =<< Ftm.getLogTime
  printFxTradeData td
  printf "\n"

printTradeResult :: Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> IO ()
printTradeResult td td' units = do
  printf "%s : " =<< Ftm.getLogTime
  printf "%7.3f %7.3f %7.3f %8d %8.0f (%+8.0f) %6d %6d %6.2f\n"
    (Ftd.profit td')
    (Fcd.close $ Ftd.rate td')
    (Fcd.close $ Ftd.chart td')
    units
    (Ftd.realizedPL td')
    (Ftd.realizedPL $ td' - td)
    (Ftd.trSuccess td')
    (Ftd.trFail td')
    (Ftd.getWinRate td')

printFxTradeData :: Ftd.FxTradeData -> IO ()
printFxTradeData td = do
  printf "| %5.1f %8.0f : %3d %3d %3.0f "
    (Ftd.profit td)
    (Ftd.realizedPL td)
    (Ftd.trSuccess td)
    (Ftd.trFail td)
    (Ftd.getWinRate td)

{-
printFxSettingData :: Fsd.FxSettingData -> IO ()
printFxSettingData fsd = do
  printf "        %10d %10d\n"
    (Fsd.trSuccess     . Fsd.learningSetting $ Fsd.fxSetting fsd)
    (Fsd.trSuccessDate . Fsd.learningSetting $ Fsd.fxSetting fsd)
  printFxTaSetting . Fsd.fxTaOpen  $ Fsd.fxSetting fsd
  printFxTaSetting . Fsd.fxTaClose $ Fsd.fxSetting fsd

printFxTaSetting :: Fad.FxAlgorithmSetting -> IO ()
printFxTaSetting fts = do
  printf "        %4d | %3d %3d %3d %3d | %3d %3d %3d %3d | %3d %3d %3d %3d | %3d %3d %3d %3d | %3d %3d %3d %3d | %3d\n%s\n%s\n"
    (Fad.simChart fts)
    (Fad.shortSetting $ Fad.rciSetting fts) (Fad.middleSetting $ Fad.rciSetting fts) (Fad.longSetting $ Fad.rciSetting fts) (Fad.prevSetting $ Fad.rciSetting fts)
    (Fad.shortSetting $ Fad.rsiSetting fts) (Fad.middleSetting $ Fad.rsiSetting fts) (Fad.longSetting $ Fad.rsiSetting fts) (Fad.prevSetting $ Fad.rsiSetting fts)
    (Fad.shortSetting $ Fad.smaSetting fts) (Fad.middleSetting $ Fad.smaSetting fts) (Fad.longSetting $ Fad.smaSetting fts) (Fad.prevSetting $ Fad.smaSetting fts)
    (Fad.shortSetting $ Fad.emaSetting fts) (Fad.middleSetting $ Fad.emaSetting fts) (Fad.longSetting $ Fad.emaSetting fts) (Fad.prevSetting $ Fad.emaSetting fts)
    (Fad.shortSetting $ Fad.wmaSetting fts) (Fad.middleSetting $ Fad.wmaSetting fts) (Fad.longSetting $ Fad.wmaSetting fts) (Fad.prevSetting $ Fad.wmaSetting fts)
    (Fad.shortSetting $ Fad.macdSetting fts)
    (show . sort . Fad.toList $ Fad.techAnaTree fts)
    (show $ Fad.algorithmListCount fts)

printFxTechnicalAnalysisData :: Fad.FxTechnicalAnalysisData -> IO ()
printFxTechnicalAnalysisData ftad = do
  printf "           %6.2f %6.2f %6.2f | %6.2f %6.2f %6.2f | %6.2f %6.2f %6.2f | %6.2f %6.2f %6.2f | %6.2f %6.2f %6.2f\n"
    (Fad.short $ Fad.rci ftad) (Fad.middle $ Fad.rci ftad) (Fad.long $ Fad.rci ftad) 
    (Fad.short $ Fad.sma ftad) (Fad.long $ Fad.sma ftad) ((Fad.short $ Fad.sma ftad) - (Fad.long $ Fad.sma ftad))
    (Fad.short $ Fad.ema ftad) (Fad.long $ Fad.ema ftad) ((Fad.short $ Fad.ema ftad) - (Fad.long $ Fad.ema ftad))
    (Fad.short $ Fad.wma ftad) (Fad.long $ Fad.wma ftad) ((Fad.short $ Fad.wma ftad) - (Fad.long $ Fad.wma ftad))
    (Fad.short $ Fad.macd ftad) (Fad.long $ Fad.macd ftad) ((Fad.short $ Fad.macd ftad) - (Fad.long $ Fad.macd ftad))
-}
