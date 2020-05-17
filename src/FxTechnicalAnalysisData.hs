{-# LANGUAGE DeriveGeneric #-}

module FxTechnicalAnalysisData
  ( FxTechnicalAnalysisData (..)
  , FxMovingAverageData (..)
  , FxTechnicalAnalysisSetting (..)
  , FxAlgorithmSetting (..)
  , FxAlMaSetting (..)
  , FxChartTaData (..)
  , FxTradePosition (..)
  , initFxTechnicalAnalysisSetting
  , initFxAlgorithmSetting
  , initTechAnaLeafData
  , initFxTechnicalAnalysisData
  , initFxMovingAverageData
  , initAlgoLeafData
  , getSimChartMax
  , setFxTechnicalAnalysisSetting
  ) where

import           Data.Hashable
import qualified Data.Map          as M
import           Debug.Trace
import qualified FxChartData       as Fcd
import           GHC.Generics      (Generic)
import qualified GlobalSettingData as Gsd
import qualified Tree              as Tr

data FxChartTaData = FxChartTaData
  { taChart     :: Fcd.FxChartData
  , open        :: M.Map Int FxTechnicalAnalysisData
  , closeProfit :: M.Map Int FxTechnicalAnalysisData
  , closeLoss   :: M.Map Int FxTechnicalAnalysisData
  }  deriving (Show, Read)

data FxTechnicalAnalysisData = FxTechnicalAnalysisData
  { chart :: Fcd.FxChartData
  , rci   :: FxMovingAverageData
  , sma   :: FxMovingAverageData
  , ema   :: FxMovingAverageData
  , macd  :: FxMovingAverageData
  , st    :: FxMovingAverageData
  , rsi   :: FxMovingAverageData
  , bbmf  :: FxMovingAverageData
  , bbco  :: FxMovingAverageData
  }  deriving (Show, Read, Generic)

data FxMovingAverageData = FxMovingAverageData
  { short      :: Double
  , middle     :: Double
  , long       :: Double
  , crossSL    :: FxTradePosition
  , crossSM    :: FxTradePosition
  , crossML    :: FxTradePosition
  , thresholdS :: FxTradePosition
  , thresholdL :: FxTradePosition
  , thresholdM :: FxTradePosition
  }  deriving (Show, Read, Generic)

data FxTradePosition = None | Buy | Sell deriving (Show, Read, Eq, Generic)

instance Hashable FxTechnicalAnalysisData where
     hashWithSalt _ _ = 0

instance Hashable FxMovingAverageData where
     hashWithSalt _ _ = 0
     
instance Hashable FxTradePosition  where
     hashWithSalt _ _ = 0

data FxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    :: Tr.TreeData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , techListCount  :: Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , treeAnaAndRate :: Int
                             , treeAnaOrRate  :: Int
                             , algoSetting    :: M.Map Int FxAlgorithmSetting
                             } deriving (Show, Read, Ord, Eq, Generic)

data FxAlgorithmSetting = FxAlgorithmSetting
  { algorithmTree      :: Tr.TreeData FxTechnicalAnalysisData
  , algorithmListCount :: Tr.LeafDataMap FxTechnicalAnalysisData
  , algorithmAndRate   :: Int
  , algorithmOrRate    :: Int
  , smaSetting         :: FxAlMaSetting
  , emaSetting         :: FxAlMaSetting
  , macdSetting        :: FxAlMaSetting
  , stSetting          :: FxAlMaSetting
  , rciSetting         :: FxAlMaSetting
  , rsiSetting         :: FxAlMaSetting
  , bbmfSetting        :: FxAlMaSetting
  , bbcoSetting        :: FxAlMaSetting
  , simChart           :: Int
  } deriving (Show, Read, Ord, Eq, Generic)

data FxAlMaSetting = FxAlMaSetting
  { shortSetting        :: Int
  , middleSetting       :: Int
  , longSetting         :: Int
  , thresholdSetting    :: Double
  , thresholdMaxSetting :: Double
  }  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable FxTechnicalAnalysisSetting
instance Hashable FxAlgorithmSetting
instance Hashable FxAlMaSetting

fxAlgorithmList :: [(FxTechnicalAnalysisData -> Bool, FxTechnicalAnalysisData -> Bool)]
fxAlgorithmList =
  concat $ replicate (Gsd.algorithmRepeat Gsd.gsd)
  [ (isBuy (crossSL    .  sma), isSell (crossSL    .  sma)) -- 1
  , (isBuy (crossSM    .  sma), isSell (crossSM    .  sma)) -- 2
  , (isBuy (crossML    .  sma), isSell (crossML    .  sma)) -- 3
  , (isBuy (crossSL    . macd), isSell (crossSL    . macd)) -- 4
  , (isBuy (crossSL    .  rci), isSell (crossSL    .  rci)) -- 5
  , (isBuy (crossSM    .  rci), isSell (crossSM    .  rci)) -- 6
  , (isBuy (crossML    .  rci), isSell (crossML    .  rci)) -- 7
  , (isBuy (crossSL    .  rsi), isSell (crossSL    .  rsi)) -- 8
  , (isBuy (crossSM    .  rsi), isSell (crossSM    .  rsi)) -- 9
  , (isBuy (crossML    .  rsi), isSell (crossML    .  rsi)) -- 10
  , (isBuy (crossSL    .   st), isSell (crossSL    .   st)) -- 11
  , (isBuy (crossSM    .   st), isSell (crossSM    .   st)) -- 12
  , (isBuy (crossML    .   st), isSell (crossML    .   st)) -- 13
  , (isBuy (thresholdS .  rci), isSell (thresholdS .  rci)) -- 14
  , (isBuy (thresholdM .  rci), isSell (thresholdM .  rci)) -- 15
  , (isBuy (thresholdL .  rci), isSell (thresholdL .  rci)) -- 16
  , (isBuy (thresholdS .  rsi), isSell (thresholdS .  rsi)) -- 17
  , (isBuy (thresholdM .  rsi), isSell (thresholdM .  rsi)) -- 18
  , (isBuy (thresholdL .  rsi), isSell (thresholdL .  rsi)) -- 19
  , (isBuy (thresholdS .   st), isSell (thresholdS .   st)) -- 20
  , (isBuy (thresholdM .   st), isSell (thresholdM .   st)) -- 21
  , (isBuy (thresholdL .   st), isSell (thresholdL .   st)) -- 22
  , (isBuy (thresholdS . bbmf), isSell (thresholdS . bbmf)) -- 23
  , (isBuy (thresholdM . bbmf), isSell (thresholdM . bbmf)) -- 24
  , (isBuy (thresholdL . bbmf), isSell (thresholdL . bbmf)) -- 25
  , (isBuy (thresholdS . bbco), isSell (thresholdS . bbco)) -- 26
  , (isBuy (thresholdM . bbco), isSell (thresholdM . bbco)) -- 27
  , (isBuy (thresholdL . bbco), isSell (thresholdL . bbco)) -- 28
  ]

isBuy :: (FxTechnicalAnalysisData -> FxTradePosition) -> FxTechnicalAnalysisData -> Bool
isBuy f fxta =
  f fxta == Buy

isSell :: (FxTechnicalAnalysisData -> FxTradePosition) -> FxTechnicalAnalysisData -> Bool
isSell f fxta =
  f fxta == Sell

initAlgoLeafData ::  [Tr.LeafData FxTechnicalAnalysisData]
initAlgoLeafData =
 map Tr.LeafData $ zip [0..] fxAlgorithmList

initTechAnaLeafData :: Int -> Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
initTechAnaLeafData x =
  Tr.LeafData (x, (isProfitInc x, isProfitDec x))

isProfitInc :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitInc n (fts, ftad) =
  M.member n ftad && M.member n fts && (Tr.evaluateTree fst (ftad M.! n) $ algorithmTree (fts M.! n))

isProfitDec :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitDec n (fts, ftad) =
  M.member n ftad && M.member n fts && (Tr.evaluateTree snd (ftad M.! n) $ algorithmTree (fts M.! n))

initFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting
initFxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    = Tr.Empty
                             , treeAnaAndRate = 1
                             , treeAnaOrRate  = 1
                             , techListCount  = Tr.LeafDataMap $ M.singleton (initTechAnaLeafData 0) (0, 0)
                             , algoSetting    = M.singleton 0 . initFxAlgorithmSetting . Tr.LeafDataMap . M.fromList .
                                                zip initAlgoLeafData $ repeat (0, 0)
                             }

initFxAlgorithmSetting :: Tr.LeafDataMap FxTechnicalAnalysisData -> FxAlgorithmSetting
initFxAlgorithmSetting alc =
  FxAlgorithmSetting { algorithmTree      = Tr.Empty
                     , algorithmAndRate   = 1
                     , algorithmOrRate    = 1
                     , algorithmListCount = alc
                     , rciSetting         = initFxAlMaSetting
                     , smaSetting         = initFxAlMaSetting
                     , emaSetting         = initFxAlMaSetting
                     , macdSetting        = initFxAlMaSetting
                     , stSetting          = initFxAlMaSetting
                     , rsiSetting         = initFxAlMaSetting
                     , bbmfSetting        = initFxAlMaSetting
                     , bbcoSetting        = initFxAlMaSetting
                     , simChart           = 1
                     }

initFxAlMaSetting :: FxAlMaSetting
initFxAlMaSetting =
  FxAlMaSetting { shortSetting        = 5                                              + Gsd.taRandomMargin Gsd.gsd
                , middleSetting       = 5                                              + Gsd.taRandomMargin Gsd.gsd +
                                        Gsd.taMiddleLongMargin Gsd.gsd + Gsd.taRandomMargin Gsd.gsd
                , longSetting         = 5                                              + Gsd.taRandomMargin Gsd.gsd +
                                        (Gsd.taMiddleLongMargin Gsd.gsd + Gsd.taRandomMargin Gsd.gsd) * 2
                , thresholdSetting    = 30
                , thresholdMaxSetting = 30
                }


initFxTechnicalAnalysisData :: FxTechnicalAnalysisData
initFxTechnicalAnalysisData =
  FxTechnicalAnalysisData { chart      = Fcd.initFxChartData
                          , rci        = initFxMovingAverageData
                          , sma        = initFxMovingAverageData
                          , ema        = initFxMovingAverageData
                          , macd       = initFxMovingAverageData
                          , st         = initFxMovingAverageData
                          , rsi        = initFxMovingAverageData
                          , bbmf       = initFxMovingAverageData
                          , bbco       = initFxMovingAverageData
                          }

initFxMovingAverageData :: FxMovingAverageData
initFxMovingAverageData =
  FxMovingAverageData { short      = 0
                      , middle     = 0
                      , long       = 0
                      , crossSL    = None
                      , crossSM    = None
                      , crossML    = None
                      , thresholdS = None
                      , thresholdL = None
                      , thresholdM = None
                      }


getSimChartMax :: FxTechnicalAnalysisSetting -> Int
getSimChartMax x =
  maximum $ M.map (simChart) $ algoSetting x

setFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting -> FxTechnicalAnalysisSetting
setFxTechnicalAnalysisSetting x =
  let mk = maximum . M.keys $ algoSetting x
      itad = map initTechAnaLeafData [0..mk]
  in x { techAnaTree   = Tr.setFunctionToTree        itad $ techAnaTree x
       , techListCount = Tr.setFunctionToLeafDataMap itad $ techListCount x
       , algoSetting   = M.map setFxAlgorithmSetting $ algoSetting x
       }

setFxAlgorithmSetting :: FxAlgorithmSetting -> FxAlgorithmSetting
setFxAlgorithmSetting x =
  x { algorithmTree      = Tr.setFunctionToTree        initAlgoLeafData $ algorithmTree   x
    , algorithmListCount = Tr.setFunctionToLeafDataMap initAlgoLeafData $ algorithmListCount x
    }


