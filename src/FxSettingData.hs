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
                } deriving (Show)

data FxSetting = 
  FxSetting { fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read, Ord, Eq)

data FxChart =
  FxChart { chart          :: [Fcd.FxChartData]
          , chartLength    :: Int
          } deriving (Show)

instance Eq FxSettingData where
  a == b = fxSetting a == fxSetting b

instance Ord FxSettingData where
  compare a b
    | fxSetting a == fxSetting b  = EQ
    | fxSetting a <= fxSetting b  = LT
    | otherwise           = GT

data FxLearningSetting = 
  FxLearningSetting { learningTestTimes :: Int
                    , gaLoopMax         :: Int
                    , gaLength          :: Int
                    , trSuccess         :: Integer
                    , trSuccessDate     :: Integer
                    } deriving (Show, Read, Eq, Ord)

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = FxChart { chart       = [Fcd.initFxChartData]
                                    , chartLength = 0
                                    }
                , learningSetting = FxLearningSetting { learningTestTimes  = 5
                                                      , gaLoopMax          = 5
                                                      , gaLength           = 5
                                                      , trSuccess          = 0
                                                      , trSuccessDate      = 0
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

