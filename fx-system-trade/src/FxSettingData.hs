module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , initFxSettingData
  , resetFxSettingData
  , nextFxSettingData
  , getLearningTestTimes
  , plusLearningTestTimes
  , setNo
  ) where

--import Debug.Trace
import qualified Data.Map                as M
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified Tree                    as Tr

data FxSettingData =
  FxSettingData { fxChart         :: FxChart
                , prevOpen        :: ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                                       M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData])
                , fxSetting       :: FxSetting
                , fxSettingLog    :: M.Map Int (FxSetting, Double, Int)
                } deriving (Show)

data FxSetting =
  FxSetting { no              :: Int
            , learningSetting :: FxLearningSetting
            , fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read)

data FxChart =
  FxChart { chart       :: [Fcd.FxChartData]
          , chartLength :: Int
          } deriving (Show)

instance Ord FxSetting where
  compare a b
    | no a == no b = EQ
    | no a <= no b = LT
    | otherwise    = GT

instance Eq FxSetting where
  a == b = no a == no b

instance Eq FxSettingData where
  a == b = fxSetting a == fxSetting b

instance Ord FxSettingData where
  compare a b
    | fxSetting a == fxSetting b  = EQ
    | fxSetting a <= fxSetting b  = LT
    | otherwise                   = GT

data FxLearningSetting =
  FxLearningSetting { learningTestTimes  :: Int
                    , trSuccess          :: Int
                    , trFail             :: Int
                    , successProfit      :: Double
                    , failProfit         :: Double
                    , trTrade            :: Integer
                    , trTradeDate        :: Integer
                    } deriving (Show, Read, Eq, Ord)

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = FxChart { chart       = [Fcd.initFxChartData]
                                    , chartLength = 0
                                    }
                , prevOpen            = ([], M.empty)
                , fxSetting = FxSetting { no = 0
                                        , learningSetting = FxLearningSetting { learningTestTimes  = 1
                                                                              , trSuccess          = 0
                                                                              , trFail             = 0
                                                                              , successProfit      = 0 
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

getLearningTestTimes :: FxSettingData -> Int
getLearningTestTimes fsd =
  learningTestTimes . learningSetting . fxSetting $ fsd

setNo :: Int -> FxSettingData -> FxSettingData
setNo n fsd = 
  fsd { fxSetting = (fxSetting fsd) {
          no = n
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

