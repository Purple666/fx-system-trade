
{-# LANGUAGE DeriveGeneric #-}

module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxSettingTemp(..)
  , initFxSettingData
  , setFxSettingDataLog
  , initFxSetting
  , initFxSettingTemp
  , getLogProfit
  , getLogProfitAve
  , setFxSettingData
  , getFxSettingLogResult
  , minFxSettingDelete
  , setHashFxSettingData
  ) where

import           Data.Hashable
import qualified Data.List               as L
import qualified Data.Map                as M
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified Ga
import           GHC.Generics            (Generic)
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr
import qualified FxTradeData             as Ftd

data FxSettingData =
  FxSettingData { fxSettingTemp :: FxSettingTemp
                , fxSetting     :: FxSetting
                , fxSettingLog  :: M.Map FxSetting (Double, Int)
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

data FxSettingTemp =
  FxSettingTemp { chart             :: [Fcd.FxChartData]
                , learningTestTime  :: Int
                , resultFxTradeData :: Ftd.FxTradeData
                , logProfit         :: Double
                , logCount          :: Int
                } deriving (Show)

instance Eq FxSettingData where
  a == b = fxSetting a == fxSetting b

instance Ord FxSettingData where
  compare a b = compare (fxSetting a) (fxSetting b)

instance Eq FxSetting where
  a == b = settingHash a == settingHash b

instance Ord FxSetting where
  compare a b = compare (settingHash a) (settingHash b)

instance Hashable FxSetting where
  hashWithSalt s (FxSetting _ _ c d e f) = s `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f

data FxLearningSetting =
  FxLearningSetting { totalTradeDate   :: Int
                    , numTraderadeDate :: Int
                    } deriving (Show, Read, Generic)

instance Hashable FxLearningSetting

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxSettingTemp = initFxSettingTemp
                , fxSetting     = initFxSetting
                , fxSettingLog  = M.empty
                }


initFxSetting :: FxSetting
initFxSetting =
  FxSetting { settingHash = 0
            , prevOpen            = ([], M.empty)
            , learningSetting = FxLearningSetting { totalTradeDate   = 0
                                                  , numTraderadeDate = 0
                                                  }
            , fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
            , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
            , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
            }

initFxSettingTemp :: FxSettingTemp
initFxSettingTemp =
  FxSettingTemp { chart             = []
                , learningTestTime  = 0
                , resultFxTradeData = Ftd.initFxTradeDataCommon
                , logProfit         = 0
                , logCount          = 0
                }

getSimChartMax :: FxSettingData -> Int
getSimChartMax fsd =
  L.maximum [ Fad.getSimChartMax . fxTaOpen        $ fxSetting fsd
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

setFxSettingData :: FxSetting -> M.Map FxSetting (Double, Int) -> FxSettingData
setFxSettingData fs fsl =
  setTreeFunction $ FxSettingData { fxSettingTemp = initFxSettingTemp
                                  , fxSetting     = fs
                                  , fxSettingLog  = fsl
                                  }

setFxSettingDataLog :: FxSettingData -> M.Map FxSetting (Double, Int) -> FxSettingData
setFxSettingDataLog fsd fsl =
  setTreeFunction $ fsd { fxSettingLog  = fsl
                        }

getLogProfit :: FxSettingData -> Double
getLogProfit fsd =
  (logProfit $ fxSettingTemp fsd)

getLogProfitAve :: FxSettingData -> Double
getLogProfitAve fsd =
  if (logCount $ fxSettingTemp fsd) == 0
  then 1
  else (logProfit $ fxSettingTemp fsd) / (fromIntegral . logCount $ fxSettingTemp fsd)

minFxSettingDelete :: M.Map FxSetting (Double, Int) -> M.Map FxSetting (Double, Int)
minFxSettingDelete fsl =
  M.fromList . L.take (Gsd.fxSettingLogNum Gsd.gsd) .
  L.sortBy (\(_, (a, a')) (_, (b, b')) -> compare (b * fromIntegral b') (a * fromIntegral a')) $
  M.toList fsl

getFxSettingLogResult :: FxSettingData -> (Double, Int, Double)
getFxSettingLogResult fsd =
  let (p, c) = M.foldl (\(ac, bc) (a, b) -> (ac + a, bc + b)) (0, 0) $ fxSettingLog fsd
  in if c == 0
     then (0, 0, 0)
     else (p, c, p / fromIntegral c)

setHashFxSettingData :: FxSettingData -> FxSettingData
setHashFxSettingData fsd = 
  fsd { fxSetting = (fxSetting fsd)
                    { settingHash = hash (fxSetting fsd)
                    }
      }

