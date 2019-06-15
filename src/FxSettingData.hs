{-# LANGUAGE DeriveGeneric #-}

module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , initFxSettingData
  , resetFxSettingData
  , nextFxSettingData
  , plusLearningTestTimes
  , initFxSetting
  , getLearningTestTime
  , getLearningTestTimes
  , getTradeHoldTime
  , getLossCutRate
  , getProfitRate
  , setFxSettingData
  , getFxSettingLogResult
  , maxFxSettingFrolLog
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
                    , trSuccess          :: Int
                    , trFail             :: Int
                    , successProfit      :: Double
                    , failProfit         :: Double
                    , trTrade            :: Integer
                    , trTradeDate        :: Integer
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

resetFxSettingData :: FxSettingData -> FxSettingData
resetFxSettingData fsd =
  fsd { fxSetting = (fxSetting fsd) { fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
                                    , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
                                    , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
                                    }
      }

nextFxSettingData :: [FxChart] -> FxSettingData -> FxSettingData
nextFxSettingData fc fsd =
  fsd { fxChart = fc
      }

{-
chartResetFxSettingData :: FxSettingData -> FxSettingData
chartResetFxSettingData fsd =
  fsd { fxChart = FxChart { chart       = 0
                          , chartLength = []
                          }
        fxSettingLog = M.mapWithKey (\k (_, _) -> 
      }
-}

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { fxSetting = (fxSetting fsd) {
          learningSetting = (learningSetting . fxSetting $ fsd) {
              learningTestTimes = (learningTestTimes . learningSetting . fxSetting $ fsd) + 1
              }
          }
      }

getLossCutRate :: FxSettingData -> Double
getLossCutRate fsd =
  let ls = learningSetting $ fxSetting fsd
  in if failProfit ls == 0 || trFail ls == 0
     then -Gsd.initalProperty Gsd.gsd
     else -(failProfit ls / (fromIntegral $ trFail ls)) * getLearningTestTimes2 fsd

getProfitRate :: FxSettingData -> Double
getProfitRate fsd =
  let ls = learningSetting $ fxSetting fsd
  in if successProfit ls == 0 || trSuccess ls == 0
     then Gsd.initalProperty Gsd.gsd
     else (successProfit ls / (fromIntegral $ trSuccess ls)) * getLearningTestTimes2 fsd

getLearningTestTime :: FxSettingData -> Int
getLearningTestTime fsd =
  let ls = learningSetting $ fxSetting fsd
  in Gsd.learningTestCount Gsd.gsd * getLearningTestTimes fsd ^ 2 *
     if trTrade ls == 0
     then getTradeHoldTime fsd
     else max (getTradeHoldTime fsd) (fromIntegral $ trTradeDate ls `div` trTrade ls)
                                                
getLearningTestTimes :: FxSettingData -> Int
getLearningTestTimes fsd =
  learningTestTimes . learningSetting $ fxSetting fsd

getLearningTestTimes2 :: FxSettingData -> Double
getLearningTestTimes2 fsd =
  fromIntegral . learningTestTimes . learningSetting $ fxSetting fsd

getTradeHoldTime :: FxSettingData -> Int
getTradeHoldTime fsd =
  getSimChartMax fsd * getLearningTestTimes fsd

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
                                  , fxSetting    = maxFxSettingFrolLog fsl
                                  , fxSettingLog = fsl                                         
                                  }

maxFxSettingFrolLog :: M.Map FxSetting (Double, Int) -> FxSetting
maxFxSettingFrolLog fsl =
  if null fsl == True
  then initFxSetting
  else head . map (\(x, (_, _)) -> x) . 
       L.sortBy (\(_, (a, a')) (_, (b, b')) -> compare (b / fromIntegral b') (a / fromIntegral a') ) $
       M.toList fsl

getFxSettingLogResult :: FxSettingData -> (Double, Int, Double)
getFxSettingLogResult fsd =
  let (p, c) = M.foldl (\(ac, bc) (a, b) -> (ac + a, bc + b)) (0, 0) $ fxSettingLog fsd
  in (p, c, p / fromIntegral c)

