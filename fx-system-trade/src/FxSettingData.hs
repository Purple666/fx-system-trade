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
                , fxSettingLog    :: M.Map FxSetting (Double, Int)
                } deriving (Show)

data FxSetting =
  FxSetting { learningSetting :: FxLearningSetting
            , fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read)

instance Eq FxSettingData where
  a == b = fxSetting a == fxSetting b

instance Ord FxSettingData where
  compare a b
    | fxSetting a == fxSetting b  = EQ
    | fxSetting a <= fxSetting b  = LT
    | otherwise                   = GT

instance Eq FxSetting where
  a == b = fxTaOpen        a == fxTaOpen        b &&
           fxTaCloseProfit a == fxTaCloseProfit b &&
           fxTaCloseLoss   a == fxTaCloseLoss   b   

instance Ord FxSetting where
  compare a b
    | fxTaOpen a        == fxTaOpen        b &&
      fxTaCloseProfit a == fxTaCloseProfit b &&
      fxTaCloseLoss   a == fxTaCloseLoss   b    = EQ
    | fxTaOpen a        <= fxTaOpen        b &&
      fxTaCloseProfit a <= fxTaCloseProfit b &&
      fxTaCloseLoss   a <= fxTaCloseLoss   b    = LT
    | otherwise                                 = GT

data FxChart =
  FxChart { chart       :: [Fcd.FxChartData]
          , chartLength :: Int
          } deriving (Show)

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
                , fxSetting = FxSetting { learningSetting = FxLearningSetting { learningTestTimes  = 1
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

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { fxSetting = (fxSetting fsd) {
          learningSetting = (learningSetting . fxSetting $ fsd) {
              learningTestTimes = (learningTestTimes . learningSetting . fxSetting $ fsd) + 1
              }
          }
      }

