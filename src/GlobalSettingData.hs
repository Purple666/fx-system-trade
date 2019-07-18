module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taMargin              :: Int
  , makeTreeCount         :: Int
  , algorithmRepeat       :: Int
  , learningTestCount     :: Int
  , countUpList           :: Double
  , taOpenAndRate         :: Double
  , taCloseProfitAndRate  :: Double
  , taCloseLossAndRate    :: Double
  , backtestLatestTime    :: Int
  , maxTradeTime          :: Int
  , fxSettingLogNum       :: Int
  , tradePracticeBearer   :: String
  , tradePracticeUrl      :: String
  , tradeProductionBearer :: String
  , tradeProductionUrl    :: String
  , quantityRate          :: Double
  , initalProperty        :: Double
  , maxUnit               :: Int
  , dbHost                :: String
  , spread                :: Double
  }

gsd :: GlobalSettingData
gsd =
  GlobalSettingData { taMargin              = 10
                    , makeTreeCount         = 1
                    , algorithmRepeat       = 3
                    , learningTestCount     = 5
                    , taOpenAndRate         = 1.2
                    , taCloseProfitAndRate  = 0.8
                    , taCloseLossAndRate    = 1
                    , countUpList           = 2
                    , quantityRate          = 5
                    , fxSettingLogNum       = 20
                    , backtestLatestTime    = 24 * 60 * 5 * 4 * 12
                    , maxTradeTime          = 24 * 60 * 5 * 4 
                    , initalProperty        = 2500000
                    , maxUnit               = 3000000
                    , spread                = 0.004
                    --, dbHost                = "openshift.flg.jp:30017"
                    , dbHost                = "mongo:27017"
                    , tradePracticeBearer   = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    , tradeProductionBearer = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    }



