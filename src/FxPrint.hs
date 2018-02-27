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
import qualified FxTradeData              as Ftd
import qualified FxTime                   as Ftm

printTestProgress :: Int -> Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> IO ()
printTestProgress n n' fsd tdt tdl tdlt pc lsf = do
  let lt  = truncate $ (fromIntegral $ Fsd.getLearningTime fsd) / (60 * 24 :: Double)     :: Int
      ltt = truncate $ (fromIntegral $ Fsd.getLearningTestTime fsd) / (60 * 24 :: Double) :: Int
  printf "%s : " =<< Ftm.getLogTime
  printf "%s-%s : %3d %4d %6.2f %4s "
    (Fcd.getDate n)
    (Fcd.getDate n')
    lt
    ltt
    (Fcd.close $ Ftd.chart tdt)
    (show (Ftd.side tdt))
  printFxTradeData tdt
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "%d %c %d\n" pc (head $ show lsf) (length $ Fsd.fxSettingLog fsd)

printLearningFxTradeData :: Fsd.FxSettingData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> IO ()
printLearningFxTradeData fsd tdl tdlt pc lsf = do
  printf "%s " =<< Ftm.getLogTime
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "%d %c %d\n" pc (head $ show lsf) (length $ Fsd.fxSettingLog fsd)

printStartTrade :: Ftd.FxTradeData  -> IO ()
printStartTrade td = do
  printf "%s " =<< Ftm.getLogTime
  printFxTradeData td
  printf "\n"

printTradeResult :: Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> IO ()
printTradeResult td td' units = do
  printf "%s : " =<< Ftm.getLogTime
  printf "%6.2f %6.2f %8d %8.0f (%+8.0f) %8.0f (%+8.0f) %6d %6d %6.2f\n"
    (Ftd.getEvaluationValue td')
    (Ftd.profit td')
    units
    (Ftd.realizedPL td')
    (Ftd.realizedPL $ td' - td)
    (Ftd.unrealizedPL td')
    (Ftd.unrealizedPL $ td' - td)
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
