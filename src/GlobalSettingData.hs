module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taMargin              :: Int
  , makeTreeCount         :: Int
  , algorithmRepeat       :: Int
  , learningTestCount     :: Int
  , countUpList           :: Double
  , maxTradeTime          :: Int
  , fxSettingLogNum       :: Int
  , gaNum                 :: Int
  , tradePracticeBearer   :: String
  , tradePracticeUrl      :: String
  , tradeProductionBearer :: String
  , tradeProductionUrl    :: String
  , quantityRate          :: Double
  , initalProperty        :: Double
  , productionMaxUnit     :: Int
  , practiceMaxUnit       :: Int
  , dbHost                :: String
  , spread                :: Double
  }

gsd :: GlobalSettingData
gsd =
  GlobalSettingData { taMargin              = 2
                    , makeTreeCount         = 1
                    , algorithmRepeat       = 2
                    , learningTestCount     = 2
                    , countUpList           = 1.3
                    , quantityRate          = 5
                    , fxSettingLogNum       = 1000
                    , gaNum                 = 10
                    , maxTradeTime          = 24 * 60 * 5 * 4 
                    , initalProperty        = 3000000
                    , productionMaxUnit     = 3000000
                    , practiceMaxUnit       = 250000
                    , spread                = 0.004
                    --, dbHost                = "openshift.flg.jp:30017"
                    , dbHost                = "mongo:27017"
                    , tradePracticeBearer   = "Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b"
                    , tradePracticeUrl      = "https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001"
                    , tradeProductionBearer = "Bearer 041fff2f1e9950579315d9a8d629ef9f-5b7c44123e8fc34c65951f4d3332b96b"
                    , tradeProductionUrl    = "https://api-fxpractice.oanda.com/v3/accounts/101-009-11751301-001"
                    }




