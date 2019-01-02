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
  ) where

import qualified Data.Map          as M
-- import           Debug.Trace
import qualified FxChartData       as Fcd
import qualified GlobalSettingData as Gsd
import qualified Tree              as Tr
import Data.List

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
  }  deriving (Show, Read, Eq)

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
  }  deriving (Show, Read, Eq)

data FxTradePosition = None | Buy | Sell deriving (Show, Read, Eq)

data FxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    :: Tr.TreeData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , techListCount  :: Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
                             , treeAnaAndRate :: Int
                             , treeAnaOrRate  :: Int
                             , algoSetting    :: M.Map Int FxAlgorithmSetting
                             } deriving (Show, Read)

instance Eq FxTechnicalAnalysisSetting where
  a == b = techAnaTree    a == techAnaTree    b &&
           treeAnaAndRate a == treeAnaAndRate b &&
           treeAnaOrRate  a == treeAnaOrRate  b &&
           (sort . M.elems $ algoSetting    a) == (sort . M.elems $ algoSetting    b)

instance Ord FxTechnicalAnalysisSetting where
  compare a b
    | techAnaTree    a == techAnaTree    b &&
      treeAnaAndRate a == treeAnaAndRate b &&
      treeAnaOrRate  a == treeAnaOrRate  b &&
      algoSetting    a == algoSetting    b    = EQ
    | techAnaTree    a <= techAnaTree    b &&
      treeAnaAndRate a <= treeAnaAndRate b &&
      treeAnaOrRate  a <= treeAnaOrRate  b &&
      algoSetting    a <= algoSetting    b    = LT
    | otherwise                               = GT

instance Eq FxAlgorithmSetting where
  a == b = algorithmAndRate a == algorithmAndRate b &&
           algorithmOrRate  a == algorithmOrRate  b &&
           smaSetting       a == smaSetting       b &&
           emaSetting       a == emaSetting       b &&
           wmaSetting       a == wmaSetting       b &&
           macdSetting      a == macdSetting      b &&
           stSetting        a == stSetting        b &&
           rciSetting       a == rciSetting       b &&
           rsiSetting       a == rsiSetting       b &&
           simChart         a == simChart         b

instance Ord FxAlgorithmSetting where
  compare a b
    | algorithmAndRate a == algorithmAndRate b &&
      algorithmOrRate  a == algorithmOrRate  b &&
      smaSetting       a == smaSetting       b &&
      emaSetting       a == emaSetting       b &&
      wmaSetting       a == wmaSetting       b &&
      macdSetting      a == macdSetting      b &&
      stSetting        a == stSetting        b &&
      rciSetting       a == rciSetting       b &&
      rsiSetting       a == rsiSetting       b &&
      simChart         a == simChart         b     = EQ
    | algorithmAndRate a <= algorithmAndRate b &&
      algorithmOrRate  a <= algorithmOrRate  b &&
      smaSetting       a <= smaSetting       b &&
      emaSetting       a <= emaSetting       b &&
      wmaSetting       a <= wmaSetting       b &&
      macdSetting      a <= macdSetting      b &&
      stSetting        a <= stSetting        b &&
      rciSetting       a <= rciSetting       b &&
      rsiSetting       a <= rsiSetting       b &&
      simChart         a <= simChart         b     = LT
    | otherwise                                    = GT

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
  } deriving (Show, Read)

data FxAlMaSetting = FxAlMaSetting
  { shortSetting        :: Int
  , middleSetting       :: Int
  , longSetting         :: Int
  , prevSetting         :: Int
  , thresholdSetting    :: Double
  , thresholdMaxSetting :: Double
  }  deriving (Show, Read, Eq, Ord)


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
  , (isBuy (crossSL    . ema), isSell (crossSL    . ema)) -- 0
  , (isBuy (crossSM    . ema), isSell (crossSM    . ema)) -- 1
  , (isBuy (crossML    . ema), isSell (crossML    . ema)) -- 2
  , (isBuy (crossSL    . wma), isSell (crossSL    . wma)) -- 6
  , (isBuy (crossSM    . wma), isSell (crossSM    . wma)) -- 7
  , (isBuy (crossML    . wma), isSell (crossML    . wma)) -- 8
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

