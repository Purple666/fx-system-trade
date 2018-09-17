module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , initFxSettingData
  , resetFxSettingData
  , nextFxSettingData
  , getGaLoopMax
  , plusGaLoopMax
  , plusLearningTestTimes
  ) where

--import Debug.Trace
import qualified Data.Map                as M
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad

data FxSettingData =
  FxSettingData { fxChart         :: FxChart
                , fxSetting       :: FxSetting
                , fxSettingLog    :: M.Map FxSetting (Double, Int)
                } deriving (Show)

data FxSetting =
  FxSetting { learningSetting :: FxLearningSetting
            , fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read, Ord, Eq)

data FxChart =
  FxChart { chart       :: [Fcd.FxChartData]
          , chartLength :: Int
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
                    , failProfitCount   :: Int
                    , failProfit        :: Double
                    , trTrade           :: Integer
                    , trTradeDate       :: Integer
                    } deriving (Show, Read, Eq, Ord)

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = FxChart { chart       = [Fcd.initFxChartData]
                                    , chartLength = 0
                                    }
                , fxSetting = FxSetting { learningSetting = FxLearningSetting { learningTestTimes  = 1
                                                                              , gaLoopMax          = 1
                                                                              , failProfitCount    = 0
                                                                              , failProfit         = 0
                                                                              , trTrade            = 0
                                                                              , trTradeDate        = 0
                                                                              }
                                        , fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
                                        }
                , fxSettingLog = M.empty
                }

resetFxSettingData :: FxSettingData -> FxSettingData
resetFxSettingData fsd =
  fsd { fxSetting = (fxSetting fsd) { fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
                                    , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
                                    , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
                                    }
      }

nextFxSettingData :: Int -> [Fcd.FxChartData] -> FxSettingData -> FxSettingData
nextFxSettingData cl c fsd =
  fsd { fxChart = FxChart { chart       = c
                          , chartLength = cl
                          }
      }

getGaLoopMax :: FxSettingData -> Int
getGaLoopMax fsd =
  gaLoopMax . learningSetting . fxSetting $ fsd

plusGaLoopMax :: FxSettingData -> FxSettingData
plusGaLoopMax fsd =
  fsd { fxSetting = (fxSetting fsd) {
          learningSetting = (learningSetting . fxSetting $ fsd) {
              gaLoopMax = (gaLoopMax . learningSetting . fxSetting $ fsd) + 1
              }
          }
      }

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { fxSetting = (fxSetting fsd) {
          learningSetting = (learningSetting . fxSetting $ fsd) {
              learningTestTimes = (learningTestTimes . learningSetting . fxSetting $ fsd) + 1
              }
          }
      }

