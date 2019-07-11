module FxPrint
  ( printProgressFxTradeData
  , printTradeResult
  , printTestProgress
  , printLearningFxTradeData
  , printBackTestResult
  ) where

import Debug.Trace
import           Control.Monad
import qualified Data.Map                as M
import qualified FxChartData             as Fcd
import qualified FxSettingData           as Fsd
import qualified FxTime                  as Ftm
import qualified FxTradeData             as Ftd
import qualified FxTechnicalAnalysis     as Ta
import qualified GlobalSettingData       as Gsd
import           Text.Printf

printTestProgress :: Fsd.FxSettingData -> Fsd.FxSettingData ->
                     Ftd.FxTradeData -> Ftd.FxTradeData -> [Ftd.FxTradeData] -> Int -> Bool -> Bool -> IO ()
printTestProgress fsd fsdo td tdt tdlt plsf lok retry = do
  let ltt = Ta.getLearningTestTime fsd
      ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  nd  <-  Fcd.getDate . Fcd.date $ Ftd.chart td
  nd' <-  Fcd.getDate . Fcd.date $ Ftd.chart tdt
  if retry
    then printf " "
    else return ()
  printf "%s : " =<< Ftm.getLogTime
  printf "%s-%s : %6d %6.2f %4s "
    nd
    nd'
    ltt
    (Fcd.close $ Ftd.chart tdt)
    (show (Ftd.side tdt))
  printFxTradeData tdt
  printFxTradeData $ sum tdlt
  printf "| %3d %c %c %3d %3d\n" plsf (head . show $ M.member (Fsd.fxSetting fsd) (Fsd.fxSettingLog fsd)) (head $ show lok) (length . M.filter (\(p, _) -> 0 < p) $ Fsd.fxSettingLog fsd) (Fsd.learningTestTimes ls)

printLearningFxTradeData :: Double -> Int -> Fsd.FxSettingData -> [Ftd.FxTradeData] -> Int -> Bool -> IO ()
printLearningFxTradeData p n fsd tdlt plsf le = do
  printf "%s " =<< Ftm.getLogTime
  printf "%10.3f " p
  printf "| %8d  " n 
  printFxTradeData $ sum tdlt
  printf "| %3d %c %3d\n" plsf (head $ show le) (length $ Fsd.fxSettingLog fsd)

printProgressFxTradeData :: Ftd.FxTradeData -> Fcd.FxChartData -> IO ()
printProgressFxTradeData td e = do
  printf "%s " =<< Ftm.getLogTime
  printf "%7.3f %7.3f %8.3f " (Fcd.close e) (Fcd.close $ Ftd.tradeRate td) ((Fcd.close $ Ftd.tradeRate td) - (Fcd.close e))
  printFxTradeData td
  printf "\n"

printTradeResult :: Ftd.FxSide -> Ftd.FxSide -> Ftd.FxTradeData -> Ftd.FxTradeData -> Int -> IO ()
printTradeResult open close td td' units = do
  printf "%s : " =<< Ftm.getLogTime
  printf "%5s %5s | "
    (show open)
    (show close)
  nd <- Fcd.getDate . Fcd.date $ Ftd.chart td'
  printf "%s %8d | "
    nd
    ((Fcd.no $ Ftd.chart td') - (Fcd.no $ Ftd.tradeRate td))
  printf "%7.3f (%+7.3f) %7.3f %7.3f %8d %10.0f (%+10.0f) %6d %6d %6.2f\n"
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
  printf "| %8.3f  %10.0f %10.0f : %4d %4d %3.0f "
  (Ftd.profit td)
  (Ftd.unrealizedPL td)
  (Ftd.realizedPL td)
  (Ftd.trSuccess td)
  (Ftd.trFail td)
  (Ftd.getWinRate td)

printBackTestResult :: String -> Double -> Int -> Int -> Fsd.FxSettingData ->  IO ()
printBackTestResult bar rpl s f fsd = do
  let (p, c, a) = Fsd.getFxSettingLogResult fsd
  printf (bar ++ " %10.0f %d - %d : %6.2f %6d %6.2f\n") rpl s f p c a

