{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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
  , initFxTechnicalAnalysisData
  , getPrepareTime
  , evaluateProfitInc
  , evaluateProfitDec
  , updateAlgorithmListCount
  , zeroFxalgorithmListCount
  , setFxTechnicalAnalysisSetting
  , setFxAlgorithmSetting
  , makeValidLeafDataMapInc
  , makeValidLeafDataMapDec
  , addFxalgorithmListCount
  ) where

import Debug.Trace
import Data.List
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
  , bbp3a       :: Double 
  , bbm3a       :: Double
  }  deriving (Show, Eq, Ord)

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
  }  deriving (Show, Eq, Ord)

data FxTradePosition = None | Buy | Sell deriving (Show, Eq, Ord)

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
                       } deriving (Show)


zeroFxalgorithmListCount :: FxalgorithmListCount
zeroFxalgorithmListCount = 
  FxalgorithmListCount { prev      = ([], M.empty)
                       , listCount = (Tr.emptyLeafDataMap, M.empty)
                       }

initFxTechnicalAnalysisData :: FxTechnicalAnalysisData 
initFxTechnicalAnalysisData =
  FxTechnicalAnalysisData { chart      = 0
                          , rci        = initFxMovingAverageData
                          , sma        = initFxMovingAverageData
                          , ema        = initFxMovingAverageData
                          , wma        = initFxMovingAverageData
                          , macd       = initFxMovingAverageData
                          , st         = initFxMovingAverageData
                          , rsi        = initFxMovingAverageData
                          , bbp3a      = 0
                          , bbm3a      = 0
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

initAlgoLeafData ::  [Tr.LeafData FxTechnicalAnalysisData]
initAlgoLeafData = 
 map Tr.LeafData $ zip [0..] fxAlgorithmList

initTechAnaLeafData :: Int -> Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)
initTechAnaLeafData x = 
  Tr.LeafData (x, (isProfitInc x, isProfitDec x))

initFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting
initFxTechnicalAnalysisSetting =
  FxTechnicalAnalysisSetting { techAnaTree    = Tr.Empty
                             , treeAnaAndRate = 1
                             , treeAnaOrRate  = 1
                             , techListCount  = Tr.LeafDataMap $ M.singleton (initTechAnaLeafData 0) 1
                             , algoSetting    = M.singleton 0 . initFxAlgorithmSetting . Tr.LeafDataMap . M.fromList $
                                                zip initAlgoLeafData (repeat 1)
                             }

setFxTechnicalAnalysisSetting :: FxTechnicalAnalysisSetting -> FxTechnicalAnalysisSetting
setFxTechnicalAnalysisSetting x =
  let mk = maximum . M.keys $ algoSetting x
      itad = map (\y -> initTechAnaLeafData y) [0..mk]
  in x { techAnaTree   = Tr.setFunctionToTree        itad $ techAnaTree x
       , techListCount = Tr.setFunctionToLeafDataMap itad $ techListCount x
       , algoSetting   = M.map setFxAlgorithmSetting $ algoSetting x
       }

setFxAlgorithmSetting :: FxAlgorithmSetting -> FxAlgorithmSetting
setFxAlgorithmSetting x =
  x { algorithmTree      = Tr.setFunctionToTree        initAlgoLeafData $ algorithmTree   x  
    , algorithmListCount = Tr.setFunctionToLeafDataMap initAlgoLeafData $ algorithmListCount x
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
  FxAlMaSetting { shortSetting     = Gsd.taMargin Gsd.gsd
                , middleSetting    = Gsd.taMargin Gsd.gsd * 2
                , longSetting      = Gsd.taMargin Gsd.gsd * 3
                , prevSetting      = Gsd.taMargin Gsd.gsd
                , thresholdSetting = 30
                , thresholdMaxSetting = 30
                }

evaluateProfitInc :: FxTechnicalAnalysisSetting -> M.Map Int FxTechnicalAnalysisData -> Bool
evaluateProfitInc fts ftad =
  Tr.evaluateTree fst (algoSetting fts, ftad) (techAnaTree fts)

evaluateProfitDec :: FxTechnicalAnalysisSetting -> M.Map Int FxTechnicalAnalysisData -> Bool
evaluateProfitDec fts ftad =
  Tr.evaluateTree snd (algoSetting fts, ftad) (techAnaTree fts)

isProfitInc :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitInc n (fts, ftad) =
  Tr.evaluateTree fst (ftad M.! n) $ algorithmTree (fts M.! n)

isProfitDec :: Int -> (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) -> Bool
isProfitDec n (fts, ftad) =
  Tr.evaluateTree snd (ftad M.! n) $ algorithmTree (fts M.! n)

checkAlgoSetting :: M.Map Int FxAlgorithmSetting ->
                    Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData) ->
                    (M.Map Int FxAlgorithmSetting, Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData))
checkAlgoSetting as tlc =
  let (as', pr) = foldl (\(acc, p) k -> let x = as M.! k
                                            (a, b) = Tr.checkLeafDataMap $ algorithmListCount x
                                            x' = x { algorithmListCount = Tr.addLeafDataMap b p
                                                   , algorithmTree = Tr.adjustTree (algorithmListCount x') (algorithmTree x)
                                                   }
                                        in (M.adjust (\_ -> x') k acc, a)) (as, Tr.emptyLeafDataMap)
                  . sort $ M.keys as
  in if not . M.null $ Tr.getLeafDataMap pr
     then let nk = (fst $ M.findMax as) + 1
          in (M.insert nk (initFxAlgorithmSetting pr) as',
              Tr.LeafDataMap . M.insert (initTechAnaLeafData nk) 1 $ Tr.getLeafDataMap tlc)
     else (as', tlc)

updateAlgorithmListCount :: (FxChartTaData -> M.Map Int FxTechnicalAnalysisData) -> [FxChartTaData] ->
                            (Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData),
                             M.Map Int (Tr.LeafDataMap FxTechnicalAnalysisData)) ->
                            FxTechnicalAnalysisSetting -> FxTechnicalAnalysisSetting
updateAlgorithmListCount f ctdl (ldlt, ldla) fts =
  let fts' = fts { techListCount = Tr.addLeafDataMap (techListCount fts) ldlt
                 , algoSetting   = M.foldrWithKey (\k x acc -> let y = acc M.! k
                                                                   y' = y { algorithmListCount = Tr.addLeafDataMap x (algorithmListCount y) }
                                                               in M.union (M.singleton k y') acc) (algoSetting fts) ldla
                 }
      as = updateThreshold f ctdl (algoSetting fts') 
      (as', tlc) = checkAlgoSetting as (techListCount fts')
  in fts' { techListCount = tlc
          , algoSetting   = as'
          }

makeValidLeafDataMapInc :: FxTechnicalAnalysisSetting ->
                           M.Map Int FxTechnicalAnalysisData ->
                           ([Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData FxTechnicalAnalysisData])
makeValidLeafDataMapInc fts ftad =
  let l = Tr.makeValidLeafDataList fst (algoSetting fts, ftad) (techAnaTree fts)
  in (l, M.fromList $ map (\x -> let n = fst $ Tr.getLeafData x
                                 in (n, Tr.makeValidLeafDataList fst (ftad M.! n) (algorithmTree $ (algoSetting fts) M.! n))) l)

makeValidLeafDataMapDec :: FxTechnicalAnalysisSetting ->
                           M.Map Int FxTechnicalAnalysisData ->
                           ([Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData FxTechnicalAnalysisData])
makeValidLeafDataMapDec fts ftad =
  let l = Tr.makeValidLeafDataList snd (algoSetting fts, ftad) (techAnaTree fts)
  in (l, M.fromList $ map (\x -> let n = fst $ Tr.getLeafData x
                                 in (n, Tr.makeValidLeafDataList snd (ftad M.! n) (algorithmTree $ (algoSetting fts) M.! n))) l)

addFxalgorithmListCount :: Double ->
                           ([Tr.LeafData (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData FxTechnicalAnalysisData]) ->
                           (Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData),
                            M.Map Int (Tr.LeafDataMap FxTechnicalAnalysisData)) -> 
                           (Tr.LeafDataMap (M.Map Int FxAlgorithmSetting, M.Map Int FxTechnicalAnalysisData),
                            M.Map Int (Tr.LeafDataMap FxTechnicalAnalysisData))
addFxalgorithmListCount p (ptat, pat) (tat, at) =
  (Tr.addValidLeafDataList p ptat tat,
    M.union (M.mapWithKey (\k x -> if M.member k at
                                   then Tr.addValidLeafDataList p x (at M.! k)
                                   else Tr.addValidLeafDataList p x $ (M.insert k Tr.emptyLeafDataMap at) M.! k) pat) at)


getThreshold :: Int ->
                [FxChartTaData] ->
                (FxTechnicalAnalysisData -> FxMovingAverageData) ->
                (FxChartTaData -> M.Map Int FxTechnicalAnalysisData) ->
                Double
getThreshold k ctdl f1 f2 =
  let l =  sort $ foldl (\acc x -> (short  . f1 $ f2 x M.! k):
                                   (middle . f1 $ f2 x M.! k): 
                                   (long   . f1 $ f2 x M.! k):acc) [] ctdl
  in last $ take (truncate $ (fromIntegral $ length l) * (Gsd.thresholdRate Gsd.gsd)) l

updateThreshold :: (FxChartTaData -> M.Map Int FxTechnicalAnalysisData) -> [FxChartTaData] -> M.Map Int FxAlgorithmSetting -> M.Map Int FxAlgorithmSetting
updateThreshold f ctdl as =
  M.mapWithKey (\k x -> x { stSetting  = (stSetting x)
                            { thresholdMaxSetting = ((getThreshold k ctdl st f) + (thresholdMaxSetting $ stSetting x)) / 2
                            }
                          , rciSetting = (rciSetting x)
                            { thresholdMaxSetting = (100 + (getThreshold k ctdl rci f) + (thresholdMaxSetting $ rciSetting x)) / 2
                            }
                          , rsiSetting = (rsiSetting x)
                            { thresholdMaxSetting = ((getThreshold k ctdl rsi f) + (thresholdMaxSetting $ rsiSetting x)) / 2
                            }
                          }) as


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
  , (isBBInc, isBBDec)                                    -- 28
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

{-
-}

isBuy :: (FxTechnicalAnalysisData -> FxTradePosition) -> FxTechnicalAnalysisData -> Bool
isBuy f fxta =
  f fxta == Buy
  

isSell :: (FxTechnicalAnalysisData -> FxTradePosition) -> FxTechnicalAnalysisData -> Bool
isSell f fxta =
  f fxta == Sell

isBBInc :: FxTechnicalAnalysisData -> Bool
isBBInc fxta = (bbp3a fxta) < (Fcd.close $ chart fxta)

isBBDec :: FxTechnicalAnalysisData -> Bool
isBBDec fxta = (Fcd.close $ chart fxta) < (bbm3a fxta)


getPrepareTime :: FxTechnicalAnalysisSetting -> Int
getPrepareTime x =
  maximum $ M.map (\a -> maximum [ (longSetting $ rciSetting a) + Gsd.taMargin Gsd.gsd
                                 , (longSetting $ smaSetting a) + Gsd.taMargin Gsd.gsd
                                 , (longSetting $ emaSetting a) + Gsd.taMargin Gsd.gsd
                                 , (longSetting $ wmaSetting a) + Gsd.taMargin Gsd.gsd
                                 , (longSetting $ rsiSetting a) + Gsd.taMargin Gsd.gsd
                                 , (longSetting $ stSetting a)  + Gsd.taMargin Gsd.gsd
                                 ] * (simChart a + Gsd.taMargin Gsd.gsd)) $ algoSetting x

