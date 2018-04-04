import qualified FxTradeData              as Ftd
import qualified GaFx
import System.IO
import System.Environment

---

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (command:_) <- getArgs
  case command of
    "debug"          -> GaFx.debug
    "backtest"       -> GaFx.backTest False 0 0
    "backtest-retry" -> GaFx.backTest True 0 0
    "trade-practice" -> GaFx.trade Ftd.Practice "trade_practice"


