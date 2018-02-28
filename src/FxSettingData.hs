module FxSettingData
  ( FxSettingData (..)
  , FxSetting (..)
  , FxLearningSetting (..)
  , FxChart (..)
  , makeSimChart
  , getPrepareTimeAll
  , initFxSettingData
  , nextFxSettingData
  , getLearningTime
  , getLearningTestTime
  , getLearningTestTimes
  , getGaLoopMax
  , getGaLength
  , plusGaLoopMax
  , plusGaLength
  , plusLearningTestTimes
  , updateLearningSetting
  , setFxSetting
  ) where

--import Debug.Trace
import qualified Data.Map                 as M
import qualified GlobalSettingData        as Gsd
import qualified FxChartData              as Fcd
import qualified FxTechnicalAnalysisData  as Fad
import qualified FxTradeData              as Ftd

data FxSettingData =
  FxSettingData { fxChart         :: FxChart
                , learningSetting :: FxLearningSetting
                , fxSetting       :: FxSetting
                , fxSettingLog    :: M.Map FxSetting (Double, Int)
                } deriving (Show, Eq, Ord)

data FxSetting = 
  FxSetting { fxTaOpen        :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseProfit :: Fad.FxTechnicalAnalysisSetting
            , fxTaCloseLoss   :: Fad.FxTechnicalAnalysisSetting
            } deriving (Show, Read, Ord, Eq)

data FxChart =
  FxChart { chart          :: [Fcd.FxChartData]
          , chartLength    :: Int
          } deriving (Show)

instance Eq FxChart where
  _ == _ = True

instance Ord FxChart where
  compare _ _ = EQ

data FxLearningSetting = 
  FxLearningSetting { learningTestTimes :: Int
                    , gaLoopMax         :: Int
                    , gaLength          :: Int
                    , trSuccess         :: Integer
                    , trSuccessDate     :: Integer
                    , learningTime      :: Int
                    } deriving (Show, Read, Eq, Ord)

initFxSettingData :: FxSettingData
initFxSettingData =
  FxSettingData { fxChart = FxChart { chart       = [Fcd.initFxChartData]
                                    , chartLength = 0
                                    }
                , learningSetting = FxLearningSetting { learningTestTimes  = 1
                                                      , gaLoopMax          = 1
                                                      , gaLength           = 1
                                                      , trSuccess          = 0
                                                      , trSuccessDate      = 0
                                                      , learningTime       = 60
                                                      }
                , fxSetting = FxSetting { fxTaOpen        = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseProfit = Fad.initFxTechnicalAnalysisSetting
                                        , fxTaCloseLoss   = Fad.initFxTechnicalAnalysisSetting
                                        }
                , fxSettingLog = M.empty
                }

nextFxSettingData :: Int -> [Fcd.FxChartData] -> FxSettingData -> FxSettingData
nextFxSettingData cl c fsd =
  fsd { fxChart = FxChart { chart       = c
                          , chartLength = cl
                          }
      }

makeSimChart :: Int -> [Fcd.FxChartData] -> [Fcd.FxChartData]
makeSimChart c x =
  filter (\a -> Fcd.date a `mod` c == 0) x

getPrepareTimeAll :: FxSettingData -> Int
getPrepareTimeAll fsd = 
  maximum [ (Fad.getPrepareTime . fxTaOpen        $ fxSetting fsd)
          , (Fad.getPrepareTime . fxTaCloseProfit $ fxSetting fsd)
          , (Fad.getPrepareTime . fxTaCloseLoss   $ fxSetting fsd)
          ]
  
getLearningTime :: FxSettingData -> Int
getLearningTime fsd = 
  learningTime $ learningSetting fsd

getLearningTestTime :: FxSettingData -> Int
getLearningTestTime fsd = 
  truncate $ (fromIntegral $ getLearningTime fsd) * getLearningTestTimes fsd

getLearningTestTimes :: FxSettingData -> Double
getLearningTestTimes fsd = 
  (log :: (Double -> Double)) . fromIntegral .  learningTestTimes $ learningSetting fsd
  -- 

updateLearningSetting :: [Fad.FxChartTaData] -> Ftd.FxTradeData -> Ftd.FxTradeData -> FxSettingData -> FxSettingData
updateLearningSetting ctdl td tdt fsd =
  let lt = if (trSuccess $ learningSetting fsd) == 0
           then learningTime $ learningSetting fsd
           else truncate $ (fromIntegral . trSuccessDate $ learningSetting fsd) * getLearningTestTimes fsd /
                (fromIntegral . trSuccess $ learningSetting fsd)
      ex = M.member (fxSetting fsd) $ fxSettingLog fsd
      p = Ftd.profit tdt - Ftd.profit td
  in fsd { learningSetting = (learningSetting fsd)
                             { trSuccess     = (trSuccess     $ learningSetting fsd) + (fromIntegral $ Ftd.trSuccess tdt)
                             , trSuccessDate = (trSuccessDate $ learningSetting fsd) + (fromIntegral $ Ftd.trSuccessDate tdt)
                             , learningTime  = lt
                             }
         , fxSetting = (fxSetting fsd)
                       { fxTaOpen         = Fad.updateAlgorithmListCount Fad.open
                                            ctdl (Fad.listCount $ Ftd.alcOpen tdt) (fxTaOpen  $ fxSetting fsd)
                       , fxTaCloseProfit  = Fad.updateAlgorithmListCount Fad.closeProfit ctdl
                                            (Fad.listCount $ Ftd.alcCloseProfit tdt) (fxTaCloseProfit $ fxSetting fsd)
                       , fxTaCloseLoss    = Fad.updateAlgorithmListCount Fad.closeLoss   ctdl
                                            (Fad.listCount $ Ftd.alcCloseLoss tdt) (fxTaCloseLoss $ fxSetting fsd)
                       }
         , fxSettingLog = if ex
                          then if 0 < p
                               then let (a, b) = fxSettingLog fsd M.! fxSetting fsd
                                    in M.insert (fxSetting fsd) (a + p, b + 1) . M.delete (fxSetting fsd) $ fxSettingLog fsd
                               else M.filter (\x -> 0 < fst x) .
                                    M.adjust (\(a, b) -> (a + p, b + 1)) (fxSetting fsd) $ fxSettingLog fsd
                          else if 0 < p
                               then M.insert (fxSetting fsd) (p, 1) $ fxSettingLog fsd
                               else fxSettingLog fsd
         }

{-
                   , learningTestTimes = if sf && 1 < (learningTestTimes $ learningSetting fsd)
                                         then (learningTestTimes $ learningSetting fsd) - 1
                                         else  learningTestTimes $ learningSetting fsd
                   , gaLength = if sf && 1 < (gaLength $ learningSetting fsd)
                                then (gaLength $ learningSetting fsd) - 1
                                else  gaLength $ learningSetting fsd
                   , gaLoopMax = if sf && 1 < (gaLoopMax $ learningSetting fsd)
                                 then (gaLoopMax $ learningSetting fsd) - 1
                                 else  gaLoopMax $ learningSetting fsd
                       
-}


setFxSetting :: FxSetting -> FxSetting
setFxSetting fts = 
  fts { fxTaOpen        = Fad.setFxTechnicalAnalysisSetting $ fxTaOpen fts
      , fxTaCloseProfit = Fad.setFxTechnicalAnalysisSetting $ fxTaCloseProfit fts
      , fxTaCloseLoss   = Fad.setFxTechnicalAnalysisSetting $ fxTaCloseLoss fts 
      }

getGaLoopMax :: FxSettingData -> Int
getGaLoopMax fsd =
  gaLoopMax $ learningSetting fsd

getGaLength :: FxSettingData -> Int
getGaLength fsd =
  gaLength $ learningSetting fsd
  
plusGaLoopMax :: FxSettingData -> FxSettingData
plusGaLoopMax fsd =
  fsd { learningSetting = (learningSetting fsd) {
          gaLoopMax = (gaLoopMax $ learningSetting fsd) + 1
          }
      }

plusLearningTestTimes :: FxSettingData -> FxSettingData
plusLearningTestTimes fsd =
  fsd { learningSetting = (learningSetting fsd) {
          learningTestTimes = (learningTestTimes $ learningSetting fsd) + 1
          }
      }

plusGaLength :: FxSettingData -> FxSettingData
plusGaLength fsd =
  fsd { learningSetting = (learningSetting fsd) {
          gaLength = (gaLength $ learningSetting fsd) + 1
          }
      }




