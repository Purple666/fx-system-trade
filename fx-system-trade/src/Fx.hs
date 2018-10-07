import qualified FxTradeData        as Ftd
import qualified GaFx
import           System.Environment
import           System.IO

---

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (command:_) <- getArgs
  case command of
    "debug"            -> GaFx.debug
    "backtest"         -> GaFx.backTest 0 0 False
    "backtest-latest"  -> GaFx.backTest 0 0 True
    "trade-practice"   -> GaFx.trade Ftd.Practice "trade_practice"
    "trade-production" -> GaFx.trade Ftd.Production "trade_production"


