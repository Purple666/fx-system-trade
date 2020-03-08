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
  GlobalSettingData { taRandomMargin        = 2
                    , taMiddleLongMargin    = 2
                    , makeTreeCount         = 2
                    , algorithmRepeat       = 2
                    , countUpList           = 2
                    , quantityRate          = 2
                    , treeAndRate           = 2
                    , treeOrRate            = 2
                    , learningTestTimes     = 20
                    , learningTestCount     = 20
                    , fxSettingLogNum       = 50
                    , gaNum                 = 30
                    , maxTradeTime          = 24 * 60
                    , initalProperty        = 1000000
                    , productionMaxUnit     = 3000000
                    , practiceMaxUnit       = 250000
                    , spread                = 0.008
                    --, dbHost                = "openshift.flg.jp:30017"
                    , dbHost                = "mongo:27017"
                    }

