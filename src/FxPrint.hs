module FxPrint
  ( printStartTrade
  , printTradeResult
  , printTestProgress
  , printLearningFxTradeData
  , printBackTestResult
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
  printf "%s " =<< Ftm.getLogTime
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

printBackTestResult :: String -> Int -> Int -> Fsd.FxSettingData ->  IO ()
printBackTestResult bar s f fsd = do
  let (p, c, a) = Fs.getFxSettingLogResult fsd   
  printf (bar ++ " %d - %d : %6.2f %6d %6.2f\n") (s + 1) f p c a

