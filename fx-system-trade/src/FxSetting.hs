module FxSetting
  ( getPrepareTimeAll
  , getLearningTime
  , getLearningTestTime
  , getLearningTestTimes
  , updateFxSettingData
  , createInitialGaData
  , copyFxSettingData
  , mutationFxSettingData
  , crossoverFxSettingData
  , resetFxSettingData
  , setFxSettingData
  , emptyFxSettingLog
  , getFxSettingLogResult
  , getSimChartMax
  , getTradeHoldTime
  , getLossCutRate
  , getProfitRate
  , updateFxSettingLog
  ) where

import           Control.Monad
import           Control.Monad.Random
import           Data.List
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxSettingData           as Fsd
import qualified FxTechnicalAnalysis     as Ta
import qualified FxTechnicalAnalysisData as Fad
import qualified FxTradeData             as Ftd
import qualified Ga
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

getLossCutRate :: Fsd.FxSettingData -> Double
getLossCutRate fsd =
  let ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  in if Fsd.failProfit ls == 0 || Fsd.trFail ls == 0
     then -Gsd.initalProperty Gsd.gsd
     else -(Fsd.failProfit ls / (fromIntegral $ Fsd.trFail ls)) * getLearningTestTimes fsd

getProfitRate :: Fsd.FxSettingData -> Double
getProfitRate fsd =
  let ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  in if Fsd.successProfit ls == 0 || Fsd.trSuccess ls == 0
     then Gsd.initalProperty Gsd.gsd
     else (Fsd.successProfit ls / (fromIntegral $ Fsd.trSuccess ls)) * getLearningTestTimes fsd

getLearningTime :: Fsd.FxSettingData -> Int
getLearningTime fsd =
  truncate $ getLearningTestTimes fsd * fromIntegral (getTradeHoldTime fsd)

getLearningTestTime :: Fsd.FxSettingData -> Int
getLearningTestTime fsd =
  truncate $ fromIntegral (getLearningTime fsd) * getLearningTestTimes fsd

getLearningTestTimes :: Fsd.FxSettingData -> Double
getLearningTestTimes fsd =
  (log :: (Double -> Double)) $ ((fromIntegral . Fsd.learningTestTimes . Fsd.learningSetting $ Fsd.fxSetting fsd) + 2)
  -- 
  
getTradeHoldTime :: Fsd.FxSettingData -> Int
getTradeHoldTime fsd =
  let ls = Fsd.learningSetting $ Fsd.fxSetting fsd
  in if Fsd.trTrade ls == 0
     then truncate $ (fromIntegral $ getSimChartMax fsd) * getLearningTestTimes fsd
     else truncate $ (((fromIntegral $ getSimChartMax fsd) + (fromIntegral $ Fsd.trTradeDate ls `div` Fsd.trTrade ls)) / 2) * getLearningTestTimes fsd

getSimChartMax :: Fsd.FxSettingData -> Int
getSimChartMax fsd =
  maximum [ Ta.getSimChartMax . Fsd.fxTaOpen        $ Fsd.fxSetting fsd
          , Ta.getSimChartMax . Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
          , Ta.getSimChartMax . Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd]

getFxSettingLogResult :: Fsd.FxSettingData -> (Double, Int, Double)
getFxSettingLogResult fsd =
  let (p, c) = M.foldl (\(ac, bc) (a, b) -> (ac + a, bc + b)) (0, 0) $ Fsd.fxSettingLog fsd
  in (p, c, p / fromIntegral c)

getPrepareTimeAll :: Fsd.FxSettingData -> Int
getPrepareTimeAll fsd =
  maximum [ Ta.getPrepareTime . Fsd.fxTaOpen        $ Fsd.fxSetting fsd
          , Ta.getPrepareTime . Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
          , Ta.getPrepareTime . Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
          ]

setTreeFunction :: Fsd.FxSettingData -> Fsd.FxSettingData
setTreeFunction fs =
  fs { Fsd.fxSetting = setFxSetting $ Fsd.fxSetting fs
     , Fsd.fxSettingLog  = M.mapKeys setFxSetting $ Fsd.fxSettingLog fs
     }

setFxSetting :: Fsd.FxSetting -> Fsd.FxSetting
setFxSetting fts =
  fts { Fsd.fxTaOpen        = Ta.setFxTechnicalAnalysisSetting $ Fsd.fxTaOpen fts
      , Fsd.fxTaCloseProfit = Ta.setFxTechnicalAnalysisSetting $ Fsd.fxTaCloseProfit fts
      , Fsd.fxTaCloseLoss   = Ta.setFxTechnicalAnalysisSetting $ Fsd.fxTaCloseLoss fts
      }

setFxSettingData :: Fsd.FxSetting -> M.Map Fsd.FxSetting (Double, Int) -> Fsd.FxSettingData
setFxSettingData fs fsl =
  setTreeFunction $ Fsd.FxSettingData { Fsd.fxChart = Fsd.FxChart { Fsd.chart       = [Fcd.initFxChartData]
                                                                  , Fsd.chartLength = 0
                                                                  }
                                      , Fsd.fxSetting    = fs
                                      , Fsd.fxSettingLog = fsl
                                      }

emptyFxSettingLog :: Fsd.FxSettingData -> Fsd.FxSettingData
emptyFxSettingLog fsd =
  fsd { Fsd.fxSettingLog    = M.empty
      }

updateFxSettingLog :: Fsd.FxSettingData -> [(Double,  Fsd.FxSetting)] -> Fsd.FxSettingData
updateFxSettingLog fsd fss =
  if (Gsd.fxSettingLogNum Gsd.gsd) < length fss
  then fsd { Fsd.fxSettingLog =  M.withoutKeys (Fsd.fxSettingLog fsd) . S.fromList . map (\(_, x) -> x) .
                                 take (length fss - Gsd.fxSettingLogNum Gsd.gsd) $
                                 sortBy (\(a, _) (b, _) -> compare a b) fss
           }
  else fsd

updateFxSettingLog2 :: M.Map Fsd.FxSetting (Double, Int) -> M.Map Fsd.FxSetting (Double, Int)
updateFxSettingLog2 fsl =
  let fss = M.toList fsl
  in M.withoutKeys fsl . fromList . map (\(x (_, _)) -> x) . take (length fss - Gsd.fxSettingLogNum Gsd.gsd) $
     sortBy (\(_, (a, _)) (_, (b, _)) -> compare a b) fss

updateFxSettingData :: [Fad.FxChartTaData] -> Ftd.FxTradeData -> Ftd.FxTradeData -> Fsd.FxSettingData -> Fsd.FxSettingData -> Fsd.FxSettingData
updateFxSettingData ctdl td tdt fsdl fsdo =
  let p = Ftd.profit tdt - Ftd.profit td
      fslu = updateFxSettingLog2 $ M.union (Fsd.fxSettingLog fsdl) (Fsd.fxSettingLog fsdo)
      fsl = M.filter (\(pp, _) -> 0 < pp) $ if M.member (Fsd.fxSetting fsdl) fslu
                                            then M.adjust (\(a, b) -> (a + p, b + 1)) (Fsd.fxSetting fsdl) fslu
                                            else M.insert (Fsd.fxSetting fsdl) (p, 1) fslu
      ls = Fsd.FxLearningSetting { Fsd.learningTestTimes = max
                                                           (Fsd.learningTestTimes . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                           (Fsd.learningTestTimes . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                 , Fsd.trTrade         = max
                                                         (Fsd.trTrade       . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.trTrade       . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                           + fromIntegral (Ftd.trTrade tdt)
                                 , Fsd.trTradeDate     = max
                                                         (Fsd.trTradeDate   . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.trTradeDate   . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                         + fromIntegral (Ftd.trTradeDate tdt)
                                 , Fsd.trSuccess       = max
                                                         (Fsd.trSuccess     . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.trSuccess     . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                         + Ftd.trSuccess tdt
                                 , Fsd.trFail          = max
                                                         (Fsd.trFail        . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.trFail        . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                         + Ftd.trFail tdt
                                 , Fsd.successProfit   = max
                                                         (Fsd.successProfit . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.successProfit . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                         + Ftd.successProfit tdt
                                 , Fsd.failProfit      = max
                                                         (Fsd.failProfit    . Fsd.learningSetting $ Fsd.fxSetting fsdl)
                                                         (Fsd.failProfit    . Fsd.learningSetting $ Fsd.fxSetting fsdo)
                                                         + Ftd.failProfit tdt
                                 }
  in if 0 < p
     then fsdl { Fsd.fxSetting = (Fsd.fxSetting fsdl)
                                { Fsd.learningSetting = ls
                                , Fsd.fxTaOpen         = Ta.updateAlgorithmListCount Fad.open
                                                         ctdl (Fad.listCount $ Ftd.alcOpen tdt) (Fsd.fxTaOpen $ Fsd.fxSetting fsdl)
                                , Fsd.fxTaCloseProfit  = Ta.updateAlgorithmListCount Fad.closeProfit ctdl
                                                         (Fad.listCount $ Ftd.alcCloseProfit tdt) (Fsd.fxTaCloseProfit $ Fsd.fxSetting fsdl)
                                , Fsd.fxTaCloseLoss    = Ta.updateAlgorithmListCount Fad.closeLoss   ctdl
                                                         (Fad.listCount $ Ftd.alcCloseLoss tdt) (Fsd.fxTaCloseLoss $ Fsd.fxSetting fsdl)
                                }
              , Fsd.fxSettingLog = fsl
              }
     else fsdl { Fsd.fxSetting = (Fsd.fxSetting fsdl)
                                { Fsd.learningSetting = ls
                                }
              , Fsd.fxSettingLog = fsl
              }

choice1 :: [Bool] -> Int -> b -> b -> b
choice1 die n a b = if die !! n then b else a

choice2 :: [Bool] -> Int -> b -> b -> b
choice2 die n a b = if die !! n then a else b

createRandomFxAlMaSetting :: MonadRandom m => Fad.FxAlMaSetting -> m Fad.FxAlMaSetting
createRandomFxAlMaSetting ix = do
  short  <- getRandomR (max 5 (Fad.shortSetting     ix - Gsd.taMargin Gsd.gsd), Fad.shortSetting     ix + Gsd.taMargin Gsd.gsd)
  middle <- getRandomR (max 5 (Fad.middleSetting    ix - Gsd.taMargin Gsd.gsd), Fad.middleSetting    ix + Gsd.taMargin Gsd.gsd)
  long   <- getRandomR (max 5 (Fad.longSetting      ix - Gsd.taMargin Gsd.gsd), Fad.longSetting      ix + Gsd.taMargin Gsd.gsd)
  prev   <- getRandomR (max 3 (Fad.prevSetting      ix - Gsd.taMargin Gsd.gsd), Fad.prevSetting      ix + Gsd.taMargin Gsd.gsd)
  ts     <- getRandomR (max 0 (Fad.thresholdSetting ix - fromIntegral (Gsd.taMargin Gsd.gsd)),
                        Fad.thresholdSetting ix + fromIntegral (Gsd.taMargin Gsd.gsd))
  return ix { Fad.shortSetting      = short
            , Fad.middleSetting     = max (short  + Gsd.taMargin Gsd.gsd) middle
            , Fad.longSetting       = max (middle + Gsd.taMargin Gsd.gsd) long
            , Fad.prevSetting       = prev
            , Fad.thresholdSetting  = min (Fad.thresholdMaxSetting ix) ts
            }

createRandomFxAlgorithmSetting :: MonadRandom m => Bool -> Fad.FxAlgorithmSetting -> m Fad.FxAlgorithmSetting
createRandomFxAlgorithmSetting reset ix = do
  taAndR <- getRandomR(max 1 (Fad.algorithmAndRate ix - Gsd.taMargin Gsd.gsd), Fad.algorithmAndRate ix + Gsd.taMargin Gsd.gsd * Gsd.taAndRate Gsd.gsd)
  taOrR  <- getRandomR(max 1 (Fad.algorithmOrRate  ix - Gsd.taMargin Gsd.gsd), Fad.algorithmOrRate  ix + Gsd.taMargin Gsd.gsd)
  at <- if reset
        then Tr.makeTree taAndR taOrR (Fad.algorithmListCount ix) Tr.Empty
        else Tr.makeTree taAndR taOrR (Fad.algorithmListCount ix) (Fad.algorithmTree ix)
  sc <- getRandomR (max 1 (Fad.simChart ix - Gsd.taMargin Gsd.gsd), Fad.simChart ix + Gsd.taMargin Gsd.gsd)
  sma  <- createRandomFxAlMaSetting $ Fad.smaSetting  ix
  ema  <- createRandomFxAlMaSetting $ Fad.emaSetting  ix
  wma  <- createRandomFxAlMaSetting $ Fad.wmaSetting  ix
  macd <- createRandomFxAlMaSetting $ Fad.macdSetting ix
  rci  <- createRandomFxAlMaSetting $ Fad.rciSetting  ix
  st   <- createRandomFxAlMaSetting $ Fad.stSetting   ix
  rsi  <- createRandomFxAlMaSetting $ Fad.rsiSetting  ix
  return $ ix { Fad.algorithmTree    = at
              , Fad.algorithmAndRate = taAndR
              , Fad.algorithmOrRate  = taOrR
              , Fad.rciSetting       = rci
              , Fad.smaSetting       = sma
              , Fad.emaSetting       = ema
              , Fad.wmaSetting       = wma
              , Fad.macdSetting      = macd
              , Fad.stSetting        = st
              , Fad.rsiSetting       = rsi
              , Fad.simChart         = sc
              }

createRandomFxTechnicalAnalysisSetting :: MonadRandom m => Bool ->
                                          Fad.FxTechnicalAnalysisSetting -> m Fad.FxTechnicalAnalysisSetting
createRandomFxTechnicalAnalysisSetting reset ix = do
  taAndR <- getRandomR(max 1 (Fad.treeAnaAndRate ix - Gsd.taMargin Gsd.gsd), Fad.treeAnaAndRate ix + Gsd.taMargin Gsd.gsd * Gsd.taAndRate Gsd.gsd)
  taOrR  <- getRandomR(max 1 (Fad.treeAnaOrRate  ix - Gsd.taMargin Gsd.gsd), Fad.treeAnaOrRate  ix + Gsd.taMargin Gsd.gsd)
  tat <- if reset
         then Tr.makeTree taAndR taOrR (Fad.techListCount ix) Tr.Empty
         else Tr.makeTree taAndR taOrR (Fad.techListCount ix) (Fad.techAnaTree ix)
  as' <- mapM (createRandomFxAlgorithmSetting reset) $ Fad.algoSetting ix
  return $ ix { Fad.techAnaTree    = tat
              , Fad.algoSetting    = as'
              , Fad.treeAnaAndRate = taAndR
              , Fad.treeAnaOrRate  = taOrR
              }

createRandomGaData :: MonadRandom m => Bool -> Fsd.FxSettingData -> m (Ga.LearningData Fsd.FxSettingData)
createRandomGaData reset ix = do
  faso  <- createRandomFxTechnicalAnalysisSetting reset . Fsd.fxTaOpen        $ Fsd.fxSetting ix
  fascp <- createRandomFxTechnicalAnalysisSetting reset . Fsd.fxTaCloseProfit $ Fsd.fxSetting ix
  fascl <- createRandomFxTechnicalAnalysisSetting reset . Fsd.fxTaCloseLoss   $ Fsd.fxSetting ix
  let fsd = ix { Fsd.fxSetting = (Fsd.fxSetting ix) { Fsd.fxTaOpen        = faso
                                                    , Fsd.fxTaCloseProfit = fascp
                                                    , Fsd.fxTaCloseLoss   = fascl
                                                    }
               }
  return $ Ga.learningData fsd

createInitialGaData :: MonadRandom m =>
                       Int ->
                       Fsd.FxSettingData ->
                       m (Ga.LearningData Fsd.FxSettingData)
createInitialGaData n ifsd =
  Ga.learningDataList <$> mapM (\_ -> createRandomGaData False ifsd) [1..n]

copyFxSettingData :: MonadRandom m =>
                     Ga.LearningData Fsd.FxSettingData ->
                     Ga.LearningData Fsd.FxSettingData ->
                     m (Ga.LearningData Fsd.FxSettingData)
copyFxSettingData x y = return $ x `mappend` y

mutationFxSettingData :: MonadRandom m =>
                         Ga.LearningData Fsd.FxSettingData ->
                         Ga.LearningData Fsd.FxSettingData ->
                         m (Ga.LearningData Fsd.FxSettingData)
mutationFxSettingData x _ =
  createInitialGaData 1 (Ga.getHeadGaData x)

resetFxSettingData :: MonadRandom m =>
                         Fsd.FxSettingData ->
                         m Fsd.FxSettingData
resetFxSettingData ifsd =
  Ga.getHeadGaData <$> createRandomGaData True ifsd

crossoverFxSettingData :: MonadRandom m =>
                          Ga.LearningData Fsd.FxSettingData ->
                          Ga.LearningData Fsd.FxSettingData ->
                          m (Ga.LearningData Fsd.FxSettingData)
crossoverFxSettingData x y = do
  let xfsd = Ga.getHeadGaData x
      yfsd = Ga.getHeadGaData y
  (oa, ob)   <- crossoverFxTechnicalAnalysisSetting (Fsd.fxTaOpen        $ Fsd.fxSetting xfsd) (Fsd.fxTaOpen        $ Fsd.fxSetting yfsd)
  (cpa, cpb) <- crossoverFxTechnicalAnalysisSetting (Fsd.fxTaCloseProfit $ Fsd.fxSetting xfsd) (Fsd.fxTaCloseProfit $ Fsd.fxSetting yfsd)
  (cla, clb) <- crossoverFxTechnicalAnalysisSetting (Fsd.fxTaCloseLoss   $ Fsd.fxSetting xfsd) (Fsd.fxTaCloseLoss   $ Fsd.fxSetting yfsd)
  let fsd1 = xfsd { Fsd.fxSetting = (Fsd.fxSetting xfsd)
                    { Fsd.fxTaOpen        = oa
                    , Fsd.fxTaCloseProfit = cpa
                    , Fsd.fxTaCloseLoss   = cla
                    }
                  }
      fsd2 = yfsd { Fsd.fxSetting = (Fsd.fxSetting yfsd)
                    { Fsd.fxTaOpen        = ob
                    , Fsd.fxTaCloseProfit = cpb
                    , Fsd.fxTaCloseLoss   = clb
                    }
                  }
  return $ Ga.learningData fsd1 `mappend` Ga.learningData fsd2

crossoverFxTechnicalAnalysisSetting :: MonadRandom m =>
                                       Fad.FxTechnicalAnalysisSetting ->
                                       Fad.FxTechnicalAnalysisSetting ->
                                       m (Fad.FxTechnicalAnalysisSetting, Fad.FxTechnicalAnalysisSetting)
crossoverFxTechnicalAnalysisSetting a b = do
  die      <- replicateM 2 $ getRandomR (True, False)
  (ta, tb) <- Tr.crossoverTree (Fad.treeAnaAndRate a) (Fad.treeAnaOrRate a)
              (Fad.techAnaTree a) (Fad.techListCount a) (Fad.techAnaTree b) (Fad.techListCount b)
  let mk = min (fst . M.findMax $ Fad.algoSetting a) (fst . M.findMax $ Fad.algoSetting b)
  oxs      <- mapM (\k -> do (a', b') <- crossoverFxAlgorithmSetting (Fad.algoSetting a M.! k) (Fad.algoSetting b M.! k)
                             return ((k, a'), (k, b'))) [0..mk]
  return ( a { Fad.techAnaTree    = ta
             , Fad.treeAnaAndRate = choice1 die 0 (Fad.treeAnaAndRate a) (Fad.treeAnaAndRate b)
             , Fad.treeAnaOrRate  = choice1 die 1 (Fad.treeAnaOrRate  a) (Fad.treeAnaOrRate  b)
             , Fad.algoSetting    = M.union (M.fromList $ map fst oxs) (Fad.algoSetting a)
             }
         , b { Fad.techAnaTree    = tb
             , Fad.treeAnaAndRate = choice2 die 0 (Fad.treeAnaAndRate a) (Fad.treeAnaAndRate b)
             , Fad.treeAnaOrRate  = choice2 die 1 (Fad.treeAnaOrRate  a) (Fad.treeAnaOrRate  b)
             , Fad.algoSetting    = M.union (M.fromList $ map snd oxs) (Fad.algoSetting b)
             }
         )

crossoverFxAlgorithmSetting :: MonadRandom m =>
                               Fad.FxAlgorithmSetting ->
                               Fad.FxAlgorithmSetting ->
                               m (Fad.FxAlgorithmSetting, Fad.FxAlgorithmSetting)
crossoverFxAlgorithmSetting a b = do
  die <- replicateM 4 $ getRandomR (True, False)
  (ta, tb)       <- Tr.crossoverTree (Fad.algorithmAndRate a) (Fad.algorithmOrRate a)
                    (Fad.algorithmTree a) (Fad.algorithmListCount a) (Fad.algorithmTree b) (Fad.algorithmListCount b)
  (rcia, rcib)   <- crossoverOrdFxAlMaSetting (Fad.rciSetting a) (Fad.rciSetting b)
  (smaa, smab)   <- crossoverOrdFxAlMaSetting (Fad.smaSetting a) (Fad.smaSetting b)

  (emaa, emab)   <- crossoverOrdFxAlMaSetting (Fad.emaSetting a) (Fad.emaSetting b)
  (wmaa, wmab)   <- crossoverOrdFxAlMaSetting (Fad.wmaSetting a) (Fad.wmaSetting b)
  (macda, macdb) <- crossoverOrdFxAlMaSetting (Fad.macdSetting a) (Fad.macdSetting b)
  (rsia, rsib)   <- crossoverOrdFxAlMaSetting (Fad.rsiSetting a) (Fad.rsiSetting b)
  return ( a { Fad.algorithmTree    = ta
             , Fad.algorithmAndRate = choice1 die 0 (Fad.algorithmAndRate a) (Fad.algorithmAndRate b)
             , Fad.algorithmOrRate  = choice1 die 1 (Fad.algorithmOrRate  a) (Fad.algorithmOrRate  b)
             , Fad.rciSetting       = rcia
             , Fad.smaSetting       = smaa
             , Fad.emaSetting       = emaa
             , Fad.wmaSetting       = wmaa
             , Fad.macdSetting      = macda
             , Fad.rsiSetting       = rsia
             , Fad.simChart         = choice1 die 3 (Fad.simChart  a) (Fad.simChart  b)
             }
         , b { Fad.algorithmTree    = tb
             , Fad.algorithmAndRate = choice2 die 0 (Fad.algorithmAndRate a) (Fad.algorithmAndRate b)
             , Fad.algorithmOrRate  = choice2 die 1 (Fad.algorithmOrRate  a) (Fad.algorithmOrRate  b)
             , Fad.rciSetting       = rcib
             , Fad.smaSetting       = smab
             , Fad.emaSetting       = emab
             , Fad.wmaSetting       = wmab
             , Fad.macdSetting      = macdb
             , Fad.rsiSetting       = rsib
             , Fad.simChart         = choice2 die 3 (Fad.simChart  a) (Fad.simChart  b)
             }
           )

crossoverOrdCalc :: (MonadRandom m, Num a, Ord a) => a -> Int -> [a] -> [a] -> m ([a], [a])
crossoverOrdCalc margin n x y = do
  die <- getRandomR (True, False)
  let (x' , y') = if die && (x !! n) + margin <= y !! (n + 1) && (y !! n) + margin <= x !! (n + 1)
                  then (take (n + 1) y ++ drop (n + 1) x , take (n + 1) x ++ drop (n + 1) y)
                  else (x , y)
  if length x - 1 == n
    then return (x , y)
    else crossoverOrdCalc margin (n + 1) x' y'

crossoverOrd :: (MonadRandom m, Num a, Ord a) => a -> [a] -> [a] -> m ([a], [a])
crossoverOrd mrg x y = do
  let n = length x
      x' = x ++ [maximum (x ++ y) + mrg + 1]
      y' = y ++ [maximum (x ++ y) + mrg + 1]
  r <- crossoverOrdCalc mrg 0 x' y'
  return (take n (fst r), take n (snd r))

crossoverOrdFxAlMaSetting :: MonadRandom m => Fad.FxAlMaSetting -> Fad.FxAlMaSetting -> m (Fad.FxAlMaSetting, Fad.FxAlMaSetting)
crossoverOrdFxAlMaSetting a b = do
  die <- replicateM 2 $ getRandomR (True, False)
  (a', b') <- crossoverOrd 1
              [Fad.shortSetting a, Fad.middleSetting a, Fad.longSetting a]
              [Fad.shortSetting b, Fad.middleSetting b, Fad.longSetting b]
  return ( a { Fad.shortSetting     = head a'
             , Fad.middleSetting    = a' !! 1
             , Fad.longSetting      = a' !! 2
             , Fad.prevSetting      = choice1 die 0 (Fad.prevSetting a) (Fad.prevSetting b)
             , Fad.thresholdSetting = choice1 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
             }
         , b { Fad.shortSetting     = head b'
             , Fad.middleSetting    = b' !! 1
             , Fad.longSetting      = b' !! 2
             , Fad.prevSetting      = choice2 die 0 (Fad.prevSetting a) (Fad.prevSetting b)
             , Fad.thresholdSetting = choice2 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
            }
         )

