module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taRandomMargin     :: Int
  , taMiddleLongMargin :: Int
  , makeTreeCount      :: Int
  , algorithmRepeat    :: Int
  , treeAndRate        :: Int
  , treeOrRate         :: Int
  , countUpList        :: Double
  , learningTestTimes  :: Int
  , learningTestCount  :: Int
  , maxTradeTime       :: Int
  , fxSettingLogNum    :: Int
  , gaNum              :: Int
  , quantityRate       :: Double
  , initalProperty     :: Double
  , productionMaxUnit  :: Int
  , practiceMaxUnit    :: Int
  , dbHost             :: String
  , spread             :: Double
  }

gsd :: GlobalSettingData
gsd =
  GlobalSettingData { taRandomMargin        = 10
                    , taMiddleLongMargin    = 10
                    , makeTreeCount         = 10
                    , algorithmRepeat       = 10
                    , countUpList           = 10
                    , quantityRate          = 10
                    , treeAndRate           = 1
                    , treeOrRate            = 1
                    , learningTestTimes     = 10
                    , learningTestCount     = 10
                    , fxSettingLogNum       = 50
                    , gaNum                 = 50
                    , maxTradeTime          = 24 * 60
                    , initalProperty        = 3000000
                    , productionMaxUnit     = 3000000
                    , practiceMaxUnit       = 250000
                    , spread                = 0.008
                    --, dbHost                = "openshift.flg.jp:30017"
                    , dbHost                = "mongo:27017"
                    }

