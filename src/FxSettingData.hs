{-# LANGUAGE DeriveGeneric #-}

module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , initFxSettingData
  , nextFxSettingData
  , plusLearningTestTimes
  , initFxSetting
  , getLearningTestTimes
  , setFxSettingData
  , getFxSettingLogResult
  , maxFxSettingFromLog
  , minFxSettingDelete
  ) where

import Debug.Trace
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified Tree                    as Tr
import qualified GlobalSettingData       as Gsd
import qualified FxTechnicalAnalysisData as Fad

data FxSettingData =
  FxSettingData { fxChart           :: [FxChart]
                , fxSetting         :: FxSetting
                , fxSettingLog      :: M.Map FxSetting (Double, Int)
                } deriving (Show)

data FxSetting =
  FxSetting { settingHash     :: Int
            , prevOpen        :: ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                                   M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData])
            , learningSetting :: FxLearningSetting
            , fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read, Generic)

instance Eq FxSettingData where
  a == b = fxSetting a == fxSetting b

instance Ord FxSettingData where
  compare a b = compare (fxSetting a) (fxSetting b) 

instance Eq FxSetting where
  a == b = settingHash a == settingHash b

instance Ord FxSetting where
  compare a b = compare (settingHash a) (settingHash b) 

instance Hashable FxSetting

data FxChart =
  FxChart { chart       :: [Fcd.FxChartData]
          , chartLength :: Int
          } deriving (Show, Ord, Eq)

data FxLearningSetting =
  FxLearningSetting { learningTestTimes  :: Int
                    , maxTradeDate       :: Int
                    } deriving (Show, Read, Eq, Ord, Generic)

instance Hashable FxLearningSetting

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = []
                , fxSetting = initFxSetting
                , fxSettingLog = M.empty
                }

initFxSetting :: FxSetting
initFxSetting =
  FxSetting { settingHash = 0
            , prevOpen            = ([], M.empty)
            , learningSetting = FxLearningSetting { learningTestTimes  = 1
                                                  , maxTradeDate       = 60
                                                  }
            , fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
            , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
            , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
            }

nextFxSettingData :: [FxChart] -> FxSettingData -> FxSettingData
nextFxSettingData fc fsd =
  fsd { fxChart = fc
      }

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { fxSetting = (fxSetting fsd) {
          learningSetting = (learningSetting . fxSetting $ fsd) {
              learningTestTimes = (learningTestTimes . learningSetting . fxSetting $ fsd) + 1
              }
          }
      }
                                                
getLearningTestTimes :: FxSettingData -> Int
getLearningTestTimes fsd =
  learningTestTimes . learningSetting $ fxSetting fsd

getSimChartMax :: FxSettingData -> Int
getSimChartMax fsd =
  maximum [ Fad.getSimChartMax . fxTaOpen        $ fxSetting fsd
          , Fad.getSimChartMax . fxTaCloseProfit $ fxSetting fsd
          , Fad.getSimChartMax . fxTaCloseLoss   $ fxSetting fsd
          ]

setFxSetting :: FxSetting -> FxSetting
setFxSetting fts =
  fts { fxTaOpen        = Fad.setFxTechnicalAnalysisSetting $ fxTaOpen fts
      , fxTaCloseProfit = Fad.setFxTechnicalAnalysisSetting $ fxTaCloseProfit fts
      , fxTaCloseLoss   = Fad.setFxTechnicalAnalysisSetting $ fxTaCloseLoss fts
      }

setTreeFunction :: FxSettingData -> FxSettingData
setTreeFunction fs =
  fs { fxSetting = setFxSetting $ fxSetting fs
     , fxSettingLog  = M.mapKeys setFxSetting $ fxSettingLog fs
     }


setFxSettingData :: M.Map FxSetting (Double, Int) -> FxSettingData
setFxSettingData  fsl =
  setTreeFunction $ FxSettingData { fxChart = []
                                  , fxSetting    = maxFxSettingFromLog fsl
                                  , fxSettingLog = fsl                                         
                                  }

maxFxSettingFromLog :: M.Map FxSetting (Double, Int) -> FxSetting
maxFxSettingFromLog fsl =
  if null fsl == True
  then initFxSetting
  else head . map (\(x, (_, _)) -> x) . 
       L.sortBy (\(_, (a, a')) (_, (b, b')) -> compare b a) $
       M.toList fsl

minFxSettingDelete :: M.Map FxSetting (Double, Int) -> M.Map FxSetting (Double, Int)
minFxSettingDelete fsl =
  M.fromList . take (Gsd.fxSettingLogNum Gsd.gsd) .
  L.sortBy (\(_, (a, a')) (_, (b, b')) -> compare b a) $
  M.toList fsl

getFxSettingLogResult :: FxSettingData -> (Double, Int, Double)
getFxSettingLogResult fsd =
  let (p, c) = M.foldl (\(ac, bc) (a, b) -> (ac + a, bc + b)) (0, 0) $ fxSettingLog fsd
  in if c == 0
     then (0, 0, 0)
     else (p, c, p / fromIntegral c)

