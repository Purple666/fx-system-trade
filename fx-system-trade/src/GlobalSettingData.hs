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
  , backtestLatestOneTime    :: Int
  , backtestLatestTime    :: Int
  , tradePracticeBearer   :: String
  , tradePracticeUrl      :: String
  , tradeProductionBearer :: String
  , tradeProductionUrl    :: String
  , quantityRate          :: Double
  , fxSettingLogNum       :: Int
  , initalProperty        :: Double
  , maxUnit               :: Int
  , dbHost                :: String
  , spread                :: Double
  }

gsd :: GlobalSettingData
gsd =
  GlobalSettingData { taMargin              = 2
                    , algorithmRepeat       = 2
                    , makeTreeCount         = 2
                    , learningTestCount     = 2
                    , taOpenAndRate         = 1.5
                    , taCloseProfitAndRate  = 1.1
                    , taCloseLossAndRate    = 0.8
                    , countUpList           = 2
                    , fxSettingLogNum       = 10
                    , quantityRate          = 5
                    , backtestLatestOneTime = 60 * 24 * 5
                    , backtestLatestTime    = 24 * 60 * 30
                    , initalProperty        = 5000000
                    , maxUnit               = 3000000
                    , spread                = 0.004
                    , dbHost                = "openshift.flg.jp:30017"
                    , tradePracticeBearer   = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    , tradeProductionBearer = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    }



