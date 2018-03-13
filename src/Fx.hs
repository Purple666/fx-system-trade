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
    "backtest"       -> GaFx.backTest False
    "backtest-retry" -> GaFx.backTest True
    "trade-practice" -> GaFx.trade Ftd.Practice "trade_practice"


