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

import qualified Data.Map          as M
import           Debug.Trace
import qualified FxChartData       as Fcd
import qualified GlobalSettingData as Gsd
import qualified Tree              as Tr
import GHC.Generics (Generic)
import Data.Hashable

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
  , wma   :: FxMovingAverageData
  , macd  :: FxMovingAverageData
  , st    :: FxMovingAverageData
  , rsi   :: FxMovingAverageData
  , bb    :: FxMovingAverageData
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

instance Hashable FxTechnicalAnalysisData
instance Hashable FxMovingAverageData
instance Hashable FxTradePosition

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
  , wmaSetting         :: FxAlMaSetting
  , macdSetting        :: FxAlMaSetting
  , stSetting          :: FxAlMaSetting
  , rciSetting         :: FxAlMaSetting
  , rsiSetting         :: FxAlMaSetting
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
  [ (isBuy (crossSL    . sma), isSell (crossSL    . sma)) -- 1
  , (isBuy (crossSM    . sma), isSell (crossSM    . sma)) -- 2
  , (isBuy (crossML    . sma), isSell (crossML    . sma)) -- 3
  , (isBuy (crossSL   . macd), isSell (crossSL   . macd)) -- 4
  , (isBuy (crossSL    . rci), isSell (crossSL    . rci)) -- 5
  , (isBuy (crossSM    . rci), isSell (crossSM    . rci)) -- 6
  , (isBuy (crossML    . rci), isSell (crossML    . rci)) -- 7
  , (isBuy (crossSL    . rsi), isSell (crossSL    . rsi)) -- 8
  , (isBuy (crossSM    . rsi), isSell (crossSM    . rsi)) -- 9
  , (isBuy (crossML    . rsi), isSell (crossML    . rsi)) -- 10
  , (isBuy (crossSL    .  st), isSell (crossSL    .  st)) -- 11
  , (isBuy (crossSM    .  st), isSell (crossSM    .  st)) -- 12
  , (isBuy (crossML    .  st), isSell (crossML    .  st)) -- 13
  , (isBuy (thresholdS . rci), isSell (thresholdS . rci)) -- 14
  , (isBuy (thresholdM . rci), isSell (thresholdM . rci)) -- 15
  , (isBuy (thresholdL . rci), isSell (thresholdL . rci)) -- 16
  , (isBuy (thresholdS . rsi), isSell (thresholdS . rsi)) -- 17
  , (isBuy (thresholdM . rsi), isSell (thresholdM . rsi)) -- 18
  , (isBuy (thresholdL . rsi), isSell (thresholdL . rsi)) -- 19
  , (isBuy (thresholdS .  st), isSell (thresholdS .  st)) -- 20
  , (isBuy (thresholdM .  st), isSell (thresholdM .  st)) -- 21
  , (isBuy (thresholdL .  st), isSell (thresholdL .  st)) -- 22
  , (isBuy (thresholdS .  bb), isSell (thresholdS .  bb)) -- 23
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
  if M.member n ftad && M.member n fts
  then Tr.evaluateTree fst (ftad M.! n) $ algorithmTree (fts M.! n)
  else False

isProfitDec :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitDec n (fts, ftad) =
  if M.member n ftad && M.member n fts
  then Tr.evaluateTree snd (ftad M.! n) $ algorithmTree (fts M.! n)
  else False

initFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting
initFxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    = Tr.Empty
                             , treeAnaAndRate = 1
                             , treeAnaOrRate  = 1
                             , techListCount  = Tr.LeafDataMap $ M.singleton (initTechAnaLeafData 0) (1, 0)
                             , algoSetting    = M.singleton 0 . initFxAlgorithmSetting . Tr.LeafDataMap . M.fromList .
                                                zip initAlgoLeafData $ repeat (1, 0)
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
                     , wmaSetting         = initFxAlMaSetting
                     , macdSetting        = initFxAlMaSetting
                     , stSetting          = initFxAlMaSetting
                     , rsiSetting         = initFxAlMaSetting
                     , simChart           = 1
                     }

initFxAlMaSetting :: FxAlMaSetting
initFxAlMaSetting =
  FxAlMaSetting { shortSetting        = 5 + Gsd.taMargin Gsd.gsd
                , middleSetting       = 5 + Gsd.taMargin Gsd.gsd * 2
                , longSetting         = 5 + Gsd.taMargin Gsd.gsd * 3
                , thresholdSetting    = 30
                , thresholdMaxSetting = 30
                }


initFxTechnicalAnalysisData :: FxTechnicalAnalysisData
initFxTechnicalAnalysisData =
  FxTechnicalAnalysisData { chart      = Fcd.initFxChartData
                          , rci        = initFxMovingAverageData
                          , sma        = initFxMovingAverageData
                          , ema        = initFxMovingAverageData
                          , wma        = initFxMovingAverageData
                          , macd       = initFxMovingAverageData
                          , st         = initFxMovingAverageData
                          , rsi        = initFxMovingAverageData
                          , bb         = initFxMovingAverageData
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
  maximum $ M.map (\a -> simChart a) $ algoSetting x

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


