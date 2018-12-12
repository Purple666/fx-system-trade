module FxPrint
  ( printProgressFxTradeData
  , printTradeResult
  , printTestProgress
  , printLearningFxTradeData
  , printBackTestResult
  ) where

import Debug.Trace
import           Control.Monad
import qualified FxChartData   as Fcd
import qualified FxSetting     as Fs
import qualified FxSettingData as Fsd
import qualified FxTime        as Ftm
import qualified FxTradeData   as Ftd
import           Text.Printf

printTestProgress :: Int -> Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> IO ()
printTestProgress n n' fsd tdt tdl tdlt plsf lsf = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
      ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  printf "%s : " =<< Ftm.getLogTime
  nd  <-  Fcd.getDate n
  nd' <-  Fcd.getDate n'
  printf "%s-%s : %6d %6d %6.2f %4s "
    nd
    nd'
    lt
    ltt
    (Fcd.close $ Ftd.chart tdt)
    (show (Ftd.side tdt))
  printFxTradeData tdt
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "| %3d %c %3d %3d\n" plsf (head $ show lsf) (length $ Fsd.fxSettingLog fsd) (Fsd.learningTestTimes ls)

printLearningFxTradeData :: Double -> Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> IO ()
printLearningFxTradeData p n fsd tdl tdlt plsf lsf = do
  let lt  = Fs.getLearningTime     fsd
      ltt = Fs.getLearningTestTime fsd
  printf "%s " =<< Ftm.getLogTime
  printf "%8.0f " p
  printf "| %8d %6d %6d " n lt ltt
  printFxTradeData tdl
  printFxTradeData $ sum tdlt
  printf "| %3d %c %3d\n" plsf (head $ show lsf) (length $ Fsd.fxSettingLog fsd)

printProgressFxTradeData :: Ftd.FxTradeData -> Fcd.FxChartData -> IO ()
printProgressFxTradeData td e = do
  printf "%s " =<< Ftm.getLogTime
  printf "%7.3f %7.3f %8.3f " (Fcd.close e) (Fcd.close $ Ftd.tradeRate td) ((Fcd.close e) - (Fcd.close $ Ftd.tradeRate td))
  printFxTradeData td
  printf "\n"

printTradeResult :: Ftd.FxSide -> Ftd.FxSide -> Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> IO ()
printTradeResult open close td td' units = do
  printf "%s : " =<< Ftm.getLogTime
  printf "%4s %4s | "
    (show open)
    (show close)
  nd <- Fcd.getDate . Fcd.date $ Ftd.chart td'
  printf "%s %8d | "
    nd
    ((Fcd.no $ Ftd.chart td') - (Fcd.no $ Ftd.tradeRate td))
  printf "%7.3f (%+7.3f) %7.3f %7.3f %8d %8.0f (%+8.0f) %6d %6d %6.2f\n"
    (Ftd.profit td')
    (Ftd.profit $ td' - td)
    (Fcd.close $ Ftd.tradeRate td')
    (Fcd.close $ Ftd.chart td')
    units
    (Ftd.realizedPL td')
    (Ftd.realizedPL $ td' - td)
    (Ftd.trSuccess td')
    (Ftd.trFail td')
    (Ftd.getWinRate td')

printFxTradeData :: Ftd.FxTradeData -> IO ()
printFxTradeData td =
  printf "| %8.3f  %8.0f %8.0f : %4d %4d %3.0f "
  (Ftd.profit td)
  (Ftd.unrealizedPL td)
  (Ftd.realizedPL td)
  (Ftd.trSuccess td)
  (Ftd.trFail td)
  (Ftd.getWinRate td)

printBackTestResult :: String -> Int -> Int -> Fsd.FxSettingData ->  IO ()
printBackTestResult bar s f fsd = do
  let (p, c, a) = Fs.getFxSettingLogResult fsd
  printf (bar ++ " %d - %d : %6.2f %6d %6.2f\n") s f p c a

