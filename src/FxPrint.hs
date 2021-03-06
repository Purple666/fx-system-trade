module FxPrint
  ( printProgressFxTradeData
  , printTradeResult
  , printTestProgress
  , printLearningFxTradeData
  , printBackTestResult
  ) where

import           Control.Monad
import qualified Data.Map            as M
import           Debug.Trace
import qualified FxChartData         as Fcd
import qualified FxSettingData       as Fsd
import qualified FxTechnicalAnalysis as Ta
import qualified FxTime              as Ftm
import qualified FxTradeData         as Ftd
import qualified GlobalSettingData   as Gsd
import           Text.Printf


printTestProgress :: Fsd.FxSettingData -> 
                     Ftd.FxTradeData -> Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> Bool -> Bool -> IO ()
printTestProgress fsd td tdt tdl oknum lok fsde = do
  let ltt = Ta.getLearningTestTime fsd
      ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  nd  <-  Fcd.getDate . Fcd.date $ Ftd.chart td
  nd' <-  Fcd.getDate . Fcd.date $ Ftd.chart tdt
  printf "%s : " =<< Ftm.getLogTime
  printf "%s-%s : %6d %6.2f %4s "
    nd
    nd'
    ltt
    (Fcd.close $ Ftd.chart tdt)
    (show (Ftd.side tdt))
  printFxTradeData tdt
  printFxTradeData . Fsd.resultFxTradeData $ Fsd.fxSettingTemp fsd
  printFxTradeData tdl
  printLearningResult fsd tdl oknum lok fsde

printLearningFxTradeData :: Fcd.FxChartData -> Fsd.FxSettingData -> Ftd.FxTradeData -> Int -> Bool -> Bool -> IO ()
printLearningFxTradeData e fsd tdl oknum lok fsde = do
  -- printFxTradeData . Fsd.resultFxTradeData $ Fsd.fxSettingTemp fsd
  printf "%s : " =<< Ftm.getLogTime
  nd <- Fcd.getDate $ Fcd.date e
  printf "%s "
    nd
  printFxTradeData tdl
  printLearningResult fsd tdl oknum lok fsde

printProgressFxTradeData :: Ftd.FxTradeData -> Fcd.FxChartData -> IO ()
printProgressFxTradeData td e = do
  printf "%s : " =<< Ftm.getLogTime
  printf "%7.3f %7.3f %8.3f " (Fcd.close e) (Fcd.close $ Ftd.tradeRate td) (Fcd.close (Ftd.tradeRate td) - Fcd.close e)
  printFxTradeData td
  printf "\n"

printTradeResult :: Ftd.FxSide -> Ftd.FxSide -> Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> IO ()
printTradeResult open close td td' units = do
  printf "%s : " =<< Ftm.getLogTime
  nd <- Fcd.getDate . Fcd.date $ Ftd.chart td'
  printf "%s %8d | "
    nd
    (Fcd.no (Ftd.chart td') - Fcd.no (Ftd.tradeRate td))
  printf "%5s %5s | "
    (show open)
    (show close)
  printf "%7.3f (%+7.3f) %7.3f %7.3f %8d %10.0f (%+10.0f) %6d %6d %6.2f\n"
    (Ftd.profit td')
    (Ftd.profit td' - Ftd.profit td)
    (Fcd.close $ Ftd.tradeRate td')
    (Fcd.close $ Ftd.chart td')
    units
    (Ftd.realizedPL td')
    (Ftd.realizedPL td' - Ftd.realizedPL td)
    (Ftd.trSuccess td')
    (Ftd.trFail td')
    (Ftd.getWinRate td')

printFxTradeData :: Ftd.FxTradeData -> IO ()
printFxTradeData td =
  printf "| %8.3f  %10.0f : %4d %4d %3.0f "
  (Ftd.profit td)
  (Ftd.realizedPL td)
  (Ftd.trSuccess td)
  (Ftd.trFail td)
  (Ftd.getWinRate td)

printLearningResult :: Fsd.FxSettingData -> Ftd.FxTradeData -> Int -> Bool -> Bool -> IO ()
printLearningResult fsd tdl oknum lok fsde = do
  let ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  printf "| %3d %c %c %3d\n" oknum (head $ show lok) (head $ show fsde) (length $ Fsd.fxSettingLog fsd)

printBackTestResult :: String -> Ftd.FxTradeData -> Int -> Int -> Fsd.FxSettingData ->  IO ()
printBackTestResult bar tdt s f fsd = do
  let (p, c, a) = Fsd.getFxSettingLogResult fsd
  printf (bar ++ " %8.3f %10.0f %d - %d : %6.2f %6d %6.2f\n") (Ftd.profit tdt) (Ftd.realizedPL tdt) s f p c a

