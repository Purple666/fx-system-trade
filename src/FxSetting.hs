module FxSetting
  ( createInitialGaData
  , copyFxSettingData
  , mutationFxSettingData
  , crossoverFxSettingData
  , updateFxSettingLog
  , setHashFxSettingData
  , gaLearningDataFromLog
  ) where

import           Control.Monad
import           Control.Monad.Random    as R
import           Data.Hashable
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxSettingData           as Fsd
import qualified FxTechnicalAnalysis     as Ta
import qualified FxTechnicalAnalysisData as Fad
import qualified FxTrade                 as Ft
import qualified FxTradeData             as Ftd
import qualified Ga
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

instance Ga.Ga Fsd.FxSettingData where
  copy              = copyFxSettingData
  mutation          = mutationFxSettingData
  crossover         = crossoverFxSettingData
  createInitialData = createInitialGaData
  learningEvaluate  = Ft.gaLearningEvaluate
  setHash           = setHashFxSettingData

gaLearningDataFromLog :: Int -> Fsd.FxSettingData -> Ftd.FxTradeData -> IO (Ga.LearningData Fsd.FxSettingData)
gaLearningDataFromLog n fsd td = do
  let fsl = Fsd.fxSettingLog fsd
      maxp = if M.null fsl
             then 1
             else L.maximum . map fst $ M.elems fsl
      maxc = if M.null fsl
             then 1
             else L.maximum . map snd $ M.elems fsl
      fsl' = if M.member (Fsd.fxSetting fsd) fsl
             then fsl
             else M.insert (Fsd.fxSetting fsd) (maxp, maxc) fsl
      minp = abs . L.minimum . map fst $ M.elems fsl'
  fsl'' <- mapM (\(fs, (p, c)) -> do (ltt, fc) <- Ft.getChart n 1 fsd { Fsd.fxSetting = fs } td
                                     let fsd' = fsd { Fsd.fxSetting = fs
                                                    , Fsd.fxSettingTemp = (Fsd.fxSettingTemp fsd) { Fsd.chart            = fc
                                                                                                  , Fsd.learningTestTime = ltt
                                                                                                  , Fsd.logProfit        = p + minp
                                                                                                  , Fsd.logCount         = c
                                                                                                  }
                                                    }
                                     return $ Ga.learningData fsd') $ M.toList fsl'
  return $ Ga.learningDataList fsl''

updateFxSettingLog :: Double -> Fsd.FxSettingData -> Fsd.FxSettingData -> Fsd.FxSettingData
updateFxSettingLog profits fsd fsdr =
  let fsl  = M.unionWith (\(p0, c0) (p1, c1) -> if c0 < c1
                                                then (p1, c1)
                                                else (p0, c0)) (Fsd.fxSettingLog fsd) (Fsd.fxSettingLog fsdr) 
      fs   = Fsd.fxSetting fsd
  in fsd { Fsd.fxSettingLog = Fsd.minFxSettingDelete $
                              if M.member fs fsl
                              then M.adjust (\(p, c) -> (p + profits, c + 1)) fs fsl
                              else if 0 < profits
                                   then M.insert fs (profits, 1) fsl
                                   else fsl
         }

choice1 :: [Bool] -> Int -> b -> b -> b
choice1 die n a b = if die !! n then b else a

choice2 :: [Bool] -> Int -> b -> b -> b
choice2 die n a b = if die !! n then a else b

createRandomFxTechnicalAnalysisSetting :: MonadRandom m =>
                                          Fad.FxTechnicalAnalysisSetting -> m Fad.FxTechnicalAnalysisSetting
createRandomFxTechnicalAnalysisSetting ix = do
  taAndR <- getRandomR(max 1 (Fad.treeAnaAndRate ix - Gsd.treeAndRate Gsd.gsd),
                       max 1 (Fad.treeAnaAndRate ix + Gsd.treeAndRate Gsd.gsd))
  taOrR  <- getRandomR(max 1 (Fad.treeAnaOrRate  ix - Gsd.treeOrRate  Gsd.gsd),
                       max 1 (Fad.treeAnaOrRate  ix + Gsd.treeOrRate  Gsd.gsd))
  tat <- Tr.makeTree taAndR taOrR (Fad.techListCount ix) (Fad.techAnaTree ix)
  as' <- R.mapM Ta.createRandomFxAlgorithmSetting $ Fad.algoSetting ix
  return $ ix { Fad.techAnaTree    = tat
              , Fad.algoSetting    = as'
              , Fad.treeAnaAndRate = taAndR
              , Fad.treeAnaOrRate  = taOrR
              }

createRandomGaData :: MonadRandom m => Fsd.FxSettingData -> m (Ga.LearningData Fsd.FxSettingData)
createRandomGaData ix = do
  faso  <- createRandomFxTechnicalAnalysisSetting . Fsd.fxTaOpen        $ Fsd.fxSetting ix
  fascp <- createRandomFxTechnicalAnalysisSetting . Fsd.fxTaCloseProfit $ Fsd.fxSetting ix
  fascl <- createRandomFxTechnicalAnalysisSetting . Fsd.fxTaCloseLoss   $ Fsd.fxSetting ix
  let fsd = ix { Fsd.fxSetting = (Fsd.fxSetting ix) { Fsd.fxTaOpen        = faso
                                                    , Fsd.fxTaCloseProfit = fascp
                                                    , Fsd.fxTaCloseLoss   = fascl
                                                    }
               }
  return $ Ga.learningData fsd

createInitialGaData :: MonadRandom m =>
                       Int ->
                       Ga.LearningData Fsd.FxSettingData ->
                       m (Ga.LearningData Fsd.FxSettingData)
createInitialGaData n x =
  (Ga.learningDataList . L.take n . L.concat) <$>
  R.mapM (\_ -> R.mapM createRandomGaData $ Ga.getGaDataList x) [0 .. (n `div` length x)]

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
  createRandomGaData $ Ga.getHeadGaData x

setHashFxSettingData :: Ga.LearningData Fsd.FxSettingData ->
                        Ga.LearningData Fsd.FxSettingData
setHashFxSettingData x =
  Ga.LearningData . L.nub . L.map (\(fsd, p) -> (Fsd.setHashFxSettingData fsd, p)) $ Ga.getLearningData x

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
  die      <- R.replicateM 2 $ R.getRandomR (True, False)
  (ta, tb) <- Tr.crossoverTree (Fad.treeAnaAndRate a) (Fad.treeAnaOrRate a)
              (Fad.techAnaTree a) (Fad.techListCount a) (Fad.techAnaTree b) (Fad.techListCount b)
  let mk = min (fst . M.findMax $ Fad.algoSetting a) (fst . M.findMax $ Fad.algoSetting b)
  oxs      <- R.mapM (\k -> do (a', b') <- crossoverFxAlgorithmSetting (Fad.algoSetting a M.! k) (Fad.algoSetting b M.! k)
                               return ((k, a'), (k, b'))) [0..mk]
  return ( a { Fad.techAnaTree    = ta
             , Fad.treeAnaAndRate = choice1 die 0 (Fad.treeAnaAndRate a) (Fad.treeAnaAndRate b)
             , Fad.treeAnaOrRate  = choice1 die 1 (Fad.treeAnaOrRate  a) (Fad.treeAnaOrRate  b)
             , Fad.algoSetting    = M.union (M.fromList $ L.map fst oxs) (Fad.algoSetting a)
             }
         , b { Fad.techAnaTree    = tb
             , Fad.treeAnaAndRate = choice2 die 0 (Fad.treeAnaAndRate a) (Fad.treeAnaAndRate b)
             , Fad.treeAnaOrRate  = choice2 die 1 (Fad.treeAnaOrRate  a) (Fad.treeAnaOrRate  b)
             , Fad.algoSetting    = M.union (M.fromList $ L.map snd oxs) (Fad.algoSetting b)
             }
         )

crossoverFxAlgorithmSetting :: MonadRandom m =>
                               Fad.FxAlgorithmSetting ->
                               Fad.FxAlgorithmSetting ->
                               m (Fad.FxAlgorithmSetting, Fad.FxAlgorithmSetting)
crossoverFxAlgorithmSetting a b = do
  die <- R.replicateM 3 $ R.getRandomR (True, False)
  (ta, tb)       <- Tr.crossoverTree (Fad.algorithmAndRate a) (Fad.algorithmOrRate a)
                    (Fad.algorithmTree a) (Fad.algorithmListCount a) (Fad.algorithmTree b) (Fad.algorithmListCount b)
  (rcia, rcib)   <- crossoverOrdFxAlMaSetting (Fad.rciSetting  a) (Fad.rciSetting  b)
  (smaa, smab)   <- crossoverOrdFxAlMaSetting (Fad.smaSetting  a) (Fad.smaSetting  b)
  (emaa, emab)   <- crossoverOrdFxAlMaSetting (Fad.emaSetting  a) (Fad.emaSetting  b)
  (macda, macdb) <- crossoverOrdFxAlMaSetting (Fad.macdSetting a) (Fad.macdSetting b)
  (rsia, rsib)   <- crossoverOrdFxAlMaSetting (Fad.rsiSetting  a) (Fad.rsiSetting  b)
  (bbmfa, bbmfb) <- crossoverFxAlMaSettingBB  (Fad.bbmfSetting a) (Fad.bbmfSetting b)
  (bbcoa, bbcob) <- crossoverFxAlMaSettingBB  (Fad.bbcoSetting a) (Fad.bbcoSetting b)
  return ( a { Fad.algorithmTree    = ta
             , Fad.algorithmAndRate = choice1 die 0 (Fad.algorithmAndRate a) (Fad.algorithmAndRate b)
             , Fad.algorithmOrRate  = choice1 die 1 (Fad.algorithmOrRate  a) (Fad.algorithmOrRate  b)
             , Fad.rciSetting       = rcia
             , Fad.smaSetting       = smaa
             , Fad.emaSetting       = emaa
             , Fad.macdSetting      = macda
             , Fad.rsiSetting       = rsia
             , Fad.bbmfSetting      = bbmfa
             , Fad.bbcoSetting      = bbcoa
             , Fad.simChart         = choice1 die 2 (Fad.simChart  a) (Fad.simChart  b)
             }
         , b { Fad.algorithmTree    = tb
             , Fad.algorithmAndRate = choice2 die 0 (Fad.algorithmAndRate a) (Fad.algorithmAndRate b)
             , Fad.algorithmOrRate  = choice2 die 1 (Fad.algorithmOrRate  a) (Fad.algorithmOrRate  b)
             , Fad.rciSetting       = rcib
             , Fad.smaSetting       = smab
             , Fad.emaSetting       = emab
             , Fad.macdSetting      = macdb
             , Fad.rsiSetting       = rsib
             , Fad.bbmfSetting      = bbmfb
             , Fad.bbcoSetting      = bbcob
             , Fad.simChart         = choice2 die 2 (Fad.simChart  a) (Fad.simChart  b)
             }
           )

crossoverOrdCalc :: (MonadRandom m, Num a, Ord a) => a -> Int -> [a] -> [a] -> m ([a], [a])
crossoverOrdCalc margin n x y = do
  die <- getRandomR (True, False)
  let (x' , y') = if die && (x !! n) + margin <= y !! (n + 1) && (y !! n) + margin <= x !! (n + 1)
                  then (L.take (n + 1) y L.++ L.drop (n + 1) x , L.take (n + 1) x L.++ L.drop (n + 1) y)
                  else (x , y)
  if L.length x - 1 == n
    then return (x , y)
    else crossoverOrdCalc margin (n + 1) x' y'

crossoverOrd :: (MonadRandom m, Num a, Ord a) => a -> [a] -> [a] -> m ([a], [a])
crossoverOrd mrg x y = do
  let n = L.length x
      x' = x L.++ [L.maximum (x L.++ y) + mrg + 1]
      y' = y L.++ [L.maximum (x L.++ y) + mrg + 1]
  r <- crossoverOrdCalc mrg 0 x' y'
  return (L.take n (fst r), L.take n (snd r))

crossoverOrdFxAlMaSetting :: MonadRandom m => Fad.FxAlMaSetting -> Fad.FxAlMaSetting -> m (Fad.FxAlMaSetting, Fad.FxAlMaSetting)
crossoverOrdFxAlMaSetting a b = do
  die <- R.replicateM 2 $ R.getRandomR (True, False)
  (a', b') <- crossoverOrd (Gsd.taMiddleLongMargin Gsd.gsd)
              [Fad.shortSetting a, Fad.middleSetting a, Fad.longSetting a]
              [Fad.shortSetting b, Fad.middleSetting b, Fad.longSetting b]
  return ( a { Fad.shortSetting     = L.head a'
             , Fad.middleSetting    = a' !! 1
             , Fad.longSetting      = a' !! 2
             , Fad.thresholdSetting = choice1 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
             }
         , b { Fad.shortSetting     = L.head b'
             , Fad.middleSetting    = b' !! 1
             , Fad.longSetting      = b' !! 2
             , Fad.thresholdSetting = choice2 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
            }
         )

crossoverFxAlMaSettingBB :: MonadRandom m => Fad.FxAlMaSetting -> Fad.FxAlMaSetting -> m (Fad.FxAlMaSetting, Fad.FxAlMaSetting)
crossoverFxAlMaSettingBB a b = do
  die <- R.replicateM 3 $ R.getRandomR (True, False)
  return ( a { Fad.shortSetting     = choice1 die 0 (Fad.shortSetting     a) (Fad.shortSetting      b)
             , Fad.middleSetting    = choice1 die 1 (Fad.middleSetting    a) (Fad.middleSetting     b)
             , Fad.longSetting      = choice1 die 2 (Fad.longSetting      a) (Fad.longSetting       b)
             }
         , b { Fad.shortSetting     = choice2 die 0 (Fad.shortSetting     a) (Fad.shortSetting      b)
             , Fad.middleSetting    = choice2 die 1 (Fad.middleSetting    a) (Fad.middleSetting     b)
             , Fad.longSetting      = choice2 die 2 (Fad.longSetting      a) (Fad.longSetting       b)
            }
         )


