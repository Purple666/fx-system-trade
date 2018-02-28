module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , initFxSettingData
  , nextFxSettingData
  , getGaLoopMax
  , getGaLength
  , plusGaLoopMax
  , plusGaLength
  , plusLearningTestTimes
  , makeSimChart
  ) where

--import Debug.Trace
import qualified Data.Map                 as M
import qualified FxChartData              as Fcd
import qualified FxTechnicalAnalysisData  as Fad

data FxSettingData =
  FxSettingData { fxChart         :: FxChart
                , learningSetting :: FxLearningSetting
                , fxSetting       :: FxSetting
                , fxSettingLog    :: M.Map FxSetting (Double, Int)
                } deriving (Show, Eq, Ord)

data FxSetting = 
  FxSetting { fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read, Ord, Eq)

data FxChart =
  FxChart { chart          :: [Fcd.FxChartData]
          , chartLength    :: Int
          } deriving (Show)

instance Eq FxChart where
  _ == _ = True

instance Ord FxChart where
  compare _ _ = EQ

data FxLearningSetting = 
  FxLearningSetting { learningTestTimes :: Int
                    , gaLoopMax         :: Int
                    , gaLength          :: Int
                    , trSuccess         :: Integer
                    , trSuccessDate     :: Integer
                    , learningTime      :: Int
                    } deriving (Show, Read, Eq, Ord)

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = FxChart { chart       = [Fcd.initFxChartData]
                                    , chartLength = 0
                                    }
                , learningSetting = FxLearningSetting { learningTestTimes  = 1
                                                      , gaLoopMax          = 1
                                                      , gaLength           = 1
                                                      , trSuccess          = 0
                                                      , trSuccessDate      = 0
                                                      , learningTime       = 60
                                                      }
                , fxSetting = FxSetting { fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
                                        }
                , fxSettingLog = M.empty
                }

nextFxSettingData :: Int -> [Fcd.FxChartData] -> FxSettingData -> FxSettingData
nextFxSettingData cl c fsd =
  fsd { fxChart = FxChart { chart       = c
                          , chartLength = cl
                          }
      }

getGaLoopMax :: FxSettingData -> Int
getGaLoopMax fsd =
  gaLoopMax $ learningSetting fsd

getGaLength :: FxSettingData -> Int
getGaLength fsd =
  gaLength $ learningSetting fsd
  
plusGaLoopMax :: FxSettingData -> FxSettingData
plusGaLoopMax fsd =
  fsd { learningSetting = (learningSetting fsd) {
          gaLoopMax = (gaLoopMax $ learningSetting fsd) + 1
          }
      }

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { learningSetting = (learningSetting fsd) {
          learningTestTimes = (learningTestTimes $ learningSetting fsd) + 1
          }
      }

plusGaLength :: FxSettingData -> FxSettingData
plusGaLength fsd =
  fsd { learningSetting = (learningSetting fsd) {
          gaLength = (gaLength $ learningSetting fsd) + 1
          }
      }

makeSimChart :: Int -> [Fcd.FxChartData] -> [Fcd.FxChartData]
makeSimChart c x =
  filter (\a -> Fcd.date a `mod` c == 0) x

