module FxTechnicalAnalysisData
  ( FxTechnicalAnalysisData (..)
  , FxMovingAverageData (..)
  , FxTechnicalAnalysisSetting (..)
  , FxAlgorithmSetting (..)
  , FxAlMaSetting (..)
  , FxChartTaData (..)
  , FxTradePosition (..)
  , FxalgorithmListCount (..)
  , initFxTechnicalAnalysisSetting
  , initFxAlgorithmSetting
  , initTechAnaLeafData
  , initFxTechnicalAnalysisData
  , initFxMovingAverageData
  , initAlgoLeafData
  , zeroFxalgorithmListCount
  ) where

import Debug.Trace
import qualified Data.Map                 as M
import qualified Tree                     as Tr
import qualified FxChartData              as Fcd
import qualified GlobalSettingData        as Gsd

data FxChartTaData = FxChartTaData
  { taChart     :: Fcd.FxChartData
  , open        :: M.Map Int FxTechnicalAnalysisData
  , closeProfit :: M.Map Int FxTechnicalAnalysisData
  , closeLoss   :: M.Map Int FxTechnicalAnalysisData
  }  deriving (Show)

data FxTechnicalAnalysisData = FxTechnicalAnalysisData
  { chart       :: Fcd.FxChartData
  , rci         :: FxMovingAverageData
  , sma         :: FxMovingAverageData
  , ema         :: FxMovingAverageData
  , wma         :: FxMovingAverageData
  , macd        :: FxMovingAverageData
  , st          :: FxMovingAverageData
  , rsi         :: FxMovingAverageData
  , bb          :: FxMovingAverageData
  }  deriving (Show)

data FxMovingAverageData = FxMovingAverageData
  { short      :: Double
  , middle     :: Double
  , long       :: Double
  , slopeS     :: Double
  , slopeL     :: Double
  , slopeM     :: Double
  , slopeSn    :: FxTradePosition
  , slopeLn    :: FxTradePosition
  , slopeMn    :: FxTradePosition
  , crossSL    :: FxTradePosition
  , crossSM    :: FxTradePosition
  , crossML    :: FxTradePosition
  , thresholdS :: FxTradePosition
  , thresholdL :: FxTradePosition
  , thresholdM :: FxTradePosition
  }  deriving (Show)

data FxTradePosition = None | Buy | Sell deriving (Show, Eq)

data FxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    :: Tr.TreeData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , techListCount  :: Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , treeAnaAndRate :: Int
                             , treeAnaOrRate  :: Int
                             , algoSetting    :: M.Map Int FxAlgorithmSetting
                             } deriving (Show, Read, Eq, Ord)

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
  } deriving (Show, Read, Eq, Ord)

data FxAlMaSetting = FxAlMaSetting
  { shortSetting        :: Int
  , middleSetting       :: Int
  , longSetting         :: Int
  , prevSetting         :: Int
  , thresholdSetting    :: Double
  , thresholdMaxSetting :: Double
  }  deriving (Show, Read, Eq, Ord)


data FxalgorithmListCount =
  FxalgorithmListCount { prev      :: ([Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)],
                                       M.Map Int [Tr.LeafData FxTechnicalAnalysisData])
                       , listCount :: (Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData),
                                       M.Map Int (Tr.LeafDataMap FxTechnicalAnalysisData))
                       } deriving (Show, Read)

fxAlgorithmList :: [(FxTechnicalAnalysisData -> Bool, FxTechnicalAnalysisData -> Bool)]
fxAlgorithmList =
  concat $ replicate (Gsd.algorithmRepeat Gsd.gsd)
  [ (isBuy (crossSL    . ema), isSell (crossSL    . ema)) -- 0
  , (isBuy (crossSM    . ema), isSell (crossSM    . ema)) -- 1
  , (isBuy (crossML    . ema), isSell (crossML    . ema)) -- 2
  , (isBuy (crossSL    . sma), isSell (crossSL    . sma)) -- 3
  , (isBuy (crossSM    . sma), isSell (crossSM    . sma)) -- 4
  , (isBuy (crossML    . sma), isSell (crossML    . sma)) -- 5
  , (isBuy (crossSL    . wma), isSell (crossSL    . wma)) -- 6
  , (isBuy (crossSM    . wma), isSell (crossSM    . wma)) -- 7
  , (isBuy (crossML    . wma), isSell (crossML    . wma)) -- 8
  , (isBuy (crossSL   . macd), isSell (crossSL   . macd)) -- 9
  , (isBuy (crossSL    . rci), isSell (crossSL    . rci)) -- 10
  , (isBuy (crossSM    . rci), isSell (crossSM    . rci)) -- 11
  , (isBuy (crossML    . rci), isSell (crossML    . rci)) -- 12
  , (isBuy (crossSL    . rsi), isSell (crossSL    . rsi)) -- 13
  , (isBuy (crossSM    . rsi), isSell (crossSM    . rsi)) -- 14
  , (isBuy (crossML    . rsi), isSell (crossML    . rsi)) -- 15
  , (isBuy (crossSL    .  st), isSell (crossSL    .  st)) -- 16
  , (isBuy (crossSM    .  st), isSell (crossSM    .  st)) -- 17
  , (isBuy (crossML    .  st), isSell (crossML    .  st)) -- 18
  , (isBuy (thresholdS . rci), isSell (thresholdS . rci)) -- 19
  , (isBuy (thresholdM . rci), isSell (thresholdM . rci)) -- 20
  , (isBuy (thresholdL . rci), isSell (thresholdL . rci)) -- 21
  , (isBuy (thresholdS . rsi), isSell (thresholdS . rsi)) -- 22
  , (isBuy (thresholdM . rsi), isSell (thresholdM . rsi)) -- 23
  , (isBuy (thresholdL . rsi), isSell (thresholdL . rsi)) -- 24
  , (isBuy (thresholdS .  st), isSell (thresholdS .  st)) -- 25
  , (isBuy (thresholdM .  st), isSell (thresholdM .  st)) -- 26
  , (isBuy (thresholdL .  st), isSell (thresholdL .  st)) -- 27
  , (isBuy (thresholdS .  bb), isSell (thresholdS .  bb))
  , (isBuy (slopeSn    . rsi), isSell (slopeSn    . rsi)) -- 29
  , (isBuy (slopeMn    . rsi), isSell (slopeMn    . rsi)) -- 30
  , (isBuy (slopeLn    . rsi), isSell (slopeLn    . rsi)) -- 31
  , (isBuy (slopeSn    .  st), isSell (slopeSn    .  st)) -- 32
  , (isBuy (slopeMn    .  st), isSell (slopeMn    .  st)) -- 33
  , (isBuy (slopeLn    .  st), isSell (slopeLn    .  st)) -- 34
  , (isBuy (slopeSn    . rci), isSell (slopeSn    . rci)) -- 35
  , (isBuy (slopeMn    . rci), isSell (slopeMn    . rci)) -- 36
  , (isBuy (slopeLn    . rci), isSell (slopeLn    . rci)) -- 37
  , (isBuy (slopeSn    . sma), isSell (slopeSn    . sma)) -- 38
  , (isBuy (slopeMn    . sma), isSell (slopeMn    . sma)) -- 39
  , (isBuy (slopeLn    . sma), isSell (slopeLn    . sma)) -- 40
  , (isBuy (slopeSn    . ema), isSell (slopeSn    . ema)) -- 41
  , (isBuy (slopeMn    . ema), isSell (slopeMn    . ema)) -- 42
  , (isBuy (slopeLn    . ema), isSell (slopeLn    . ema)) -- 43
  , (isBuy (slopeSn    . wma), isSell (slopeSn    . wma)) -- 44
  , (isBuy (slopeMn    . wma), isSell (slopeMn    . wma)) -- 45
  , (isBuy (slopeLn    . wma), isSell (slopeLn    . wma)) -- 46
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
  Tr.evaluateTree fst (ftad M.! n) $ algorithmTree (fts M.! n)

isProfitDec :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitDec n (fts, ftad) =
  Tr.evaluateTree snd (ftad M.! n) $ algorithmTree (fts M.! n)

initFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting
initFxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    = Tr.Empty
                             , treeAnaAndRate = 1
                             , treeAnaOrRate  = 1
                             , techListCount  = Tr.LeafDataMap $ M.singleton (initTechAnaLeafData 0) 1
                             , algoSetting    = M.singleton 0 . initFxAlgorithmSetting . Tr.LeafDataMap . M.fromList $
                                                zip initAlgoLeafData (repeat 1)
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
  FxAlMaSetting { shortSetting        = Gsd.taMargin Gsd.gsd
                , middleSetting       = Gsd.taMargin Gsd.gsd * 2
                , longSetting         = Gsd.taMargin Gsd.gsd * 3
                , prevSetting         = Gsd.taMargin Gsd.gsd
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
                      , slopeS     = 0
                      , slopeM     = 0
                      , slopeL     = 0
                      , slopeSn    = None
                      , slopeMn    = None
                      , slopeLn    = None
                      , crossSL    = None
                      , crossSM    = None
                      , crossML    = None
                      , thresholdS = None
                      , thresholdL = None
                      , thresholdM = None
                      }  

zeroFxalgorithmListCount :: FxalgorithmListCount
zeroFxalgorithmListCount = 
  FxalgorithmListCount { prev      = ([], M.empty)
                       , listCount = (Tr.emptyLeafDataMap, M.empty)
                       }
  
