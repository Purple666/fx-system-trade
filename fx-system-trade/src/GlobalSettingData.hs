module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taMargin              :: Int
  , makeTreeCount         :: Int
  , algorithmRepeat       :: Int
  , learningTestCount     :: Int
  , countUpList           :: Double
  , countUpListMax        :: Double
  , taAndRate             :: Int
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
  GlobalSettingData { taMargin              = 5
                    , learningTestCount     = 3
                    , algorithmRepeat       = 5
                    , makeTreeCount         = 5 
                    , taAndRate             = 2
                    , countUpList           = 2
                    , countUpListMax        = 3
                    , fxSettingLogNum       = 5
                    , quantityRate          = 5
                    , backtestLatestTime    = 60
                    , initalProperty        = 5000000
                    , maxUnit               = 3000000
                    , spread                = 0.004
                    , dbHost                = "openshift.flg.jp:30017"
                    , tradePracticeBearer   = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    , tradeProductionBearer = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    }

