module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taMargin                 :: Int
  , makeTreeCount            :: Int
  , algorithmRepeat          :: Int
  , learningTestCount        :: Int
  , thresholdRate            :: Double
  , countUpListMax           :: Int
  , maxTradePeriod           :: Int
  , maxLearningTime          :: Int
  , tradePracticeBearer      :: String
  , tradePracticeUrl         :: String
  , tradeProductionBearer    :: String
  , tradeProductionUrl       :: String
  , quantityRate             :: Double
  , initalProperty           :: Double
  , maxUnit                  :: Int
  , dbHost                   :: String
  , spread                   :: Double
  }

gsd :: GlobalSettingData
gsd = 
  GlobalSettingData { taMargin              = 1
                    , makeTreeCount         = 3
                    , algorithmRepeat       = 3
                    , learningTestCount     = 3
                    , countUpListMax        = 3
                    , quantityRate          = 5
                    , thresholdRate         = 0.05
                    , maxTradePeriod        = 24 * 60 * 5 * 4 * 3
                    , maxLearningTime       = 24 * 60 * 5 * 4
                    , initalProperty        = 500000
                    , maxUnit               = 3000000
                    , spread                = 0.02
                    , dbHost                = "fx-mongo"
                    , tradePracticeBearer   = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    , tradeProductionBearer = "Bearer b8831e8a4c0974fc5207eb9c4d844845-96edf86dff693710db11e5a333c18117"
                    , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v1/accounts/6716490"
                    }

