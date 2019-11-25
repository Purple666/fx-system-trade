module GlobalSettingData where

data GlobalSettingData = GlobalSettingData
  { taRandomMargin     :: Int
  , taMiddleLongMargin :: Int
  , makeTreeCount      :: Int
  , algorithmRepeat    :: Int
  , treeAndRate        :: Int
  , treeOrRate         :: Int
  , countUpList        :: Double
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
                    , taMiddleLongMargin    = 6
                    , makeTreeCount         = 5
                    , algorithmRepeat       = 5
                    , countUpList           = 2
                    , quantityRate          = 5
                    , treeAndRate           = 1
                    , treeOrRate            = 2
                    , fxSettingLogNum       = 100
                    , gaNum                 = 30
                    , maxTradeTime          = 24 * 60 * 5 * 4 * 3
                    , initalProperty        = 3000000
                    , productionMaxUnit     = 3000000
                    , practiceMaxUnit       = 250000
                    , spread                = 0.008
                    , dbHost                = "openshift.flg.jp:30017"
                    --, dbHost                = "mongo:27017"
                    }




