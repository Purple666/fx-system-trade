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
    "backtest"         -> GaFx.backTest "normal" False False
    "backtest-retry"   -> GaFx.backTest "retry"  False True
    "backtest-latest"  -> GaFx.backTest "latest" True False
    "trade-practice"   -> GaFx.trade Ftd.Practice "trade_practice"
    "trade-production" -> GaFx.trade Ftd.Production "trade_production"


