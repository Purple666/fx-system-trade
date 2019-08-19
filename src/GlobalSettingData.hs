module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taMargin              :: Int
  , makeTreeCount         :: Int
  , algorithmRepeat       :: Int
  , learningTestCount     :: Int
  , treeAndRate           :: Int
  , treeOrRate            :: Int
  , countUpList           :: Double
  , maxTradeTime          :: Int
  , fxSettingLogNum       :: Int
  , gaNum                 :: Int
  , quantityRate          :: Double
  , initalProperty        :: Double
  , productionMaxUnit     :: Int
  , practiceMaxUnit       :: Int
  , dbHost                :: String
  , spread                :: Double
  }

gsd :: GlobalSettingData
gsd =
  GlobalSettingData { taMargin              = 3
                    , makeTreeCount         = 3
                    , algorithmRepeat       = 3
                    , learningTestCount     = 3
                    , countUpList           = 3
                    , quantityRate          = 5
                    , treeAndRate           = 3
                    , treeOrRate            = 1
                    , fxSettingLogNum       = 1000
                    , gaNum                 = 20
                    , maxTradeTime          = 24 * 60 * 5 * 4 * 3
                    , initalProperty        = 3000000
                    , productionMaxUnit     = 3000000
                    , practiceMaxUnit       = 250000
                    , spread                = 0.000
                    --, dbHost                = "openshift.flg.jp:30017"
                    , dbHost                = "mongo:27017"
                    }




