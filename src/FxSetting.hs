module FxSetting
  ( createInitialGaData
  , copyFxSettingData
  , mutationFxSettingData
  , crossoverFxSettingData
  , resetFxSettingData
  , updateFxSettingLog
  , setHashFxSettingData
  ) where

import           Control.Monad
import           Control.Monad.Random
import           Data.Hashable
import qualified Data.List               as L
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
import qualified FxTrade                 as Ft

instance Ga.Ga Fsd.FxSettingData where
  copy              = copyFxSettingData
  mutation          = mutationFxSettingData
  crossover         = crossoverFxSettingData
  createInitialData = createInitialGaData
  learningEvaluate  = Ft.gaLearningEvaluate
  setHash           = setHashFxSettingData

updateFxSettingLog :: Double -> Fsd.FxSettingData -> Fsd.FxSettingData -> Fsd.FxSettingData
updateFxSettingLog profits fsd fsdf = 
  let fsl  = M.unionWith (\(a, b) (a', b') -> if b < b'
                                              then (a', b')
                                              else (a, b)) (Fsd.fxSettingLog fsd) (Fsd.fxSettingLog fsdf)
      fs   = Fsd.fxSetting fsd
      fsl' = if M.member fs fsl
             then let (p, c) = fsl M.! fs
                  in if 0 < p + profits
                     then M.insert fs (p + profits, c + 1) $ M.delete fs fsl
                     else M.delete fs fsl
             else if 0 < profits
                  then M.insert fs (profits, 1) fsl
                  else fsl
      fsd' = if length fsl' < length fsl && 0 < length fsl'
             then fsd { Fsd.fxSetting = Fsd.maxFxSettingFrolLog fsl'
                      }
             else fsd
  in fsd' { Fsd.fxSettingLog = fsl'
          }
  
choice1 :: [Bool] -> Int -> b -> b -> b
choice1 die n a b = if die !! n then b else a

choice2 :: [Bool] -> Int -> b -> b -> b
choice2 die n a b = if die !! n then a else b

createRandomFxAlMaSetting :: MonadRandom m => Fad.FxAlMaSetting -> m Fad.FxAlMaSetting
createRandomFxAlMaSetting ix = do
  short  <- getRandomR (max 5 (Fad.shortSetting     ix - Gsd.taMargin Gsd.gsd), 5 + Fad.shortSetting     ix + Gsd.taMargin Gsd.gsd)
  middle <- getRandomR (max 5 (Fad.middleSetting    ix - Gsd.taMargin Gsd.gsd), 5 + Fad.middleSetting    ix + Gsd.taMargin Gsd.gsd)
  long   <- getRandomR (max 5 (Fad.longSetting      ix - Gsd.taMargin Gsd.gsd), 5 + Fad.longSetting      ix + Gsd.taMargin Gsd.gsd)
  ts     <- getRandomR (0, Fad.thresholdSetting ix + fromIntegral (Gsd.taMargin Gsd.gsd))
  return ix { Fad.shortSetting      = short
            , Fad.middleSetting     = max (short  + Gsd.taMargin Gsd.gsd) middle
            , Fad.longSetting       = max (middle + Gsd.taMargin Gsd.gsd) long
            , Fad.thresholdSetting  = min (Fad.thresholdMaxSetting ix) ts
            }

createRandomFxAlgorithmSetting :: MonadRandom m => Bool -> Double -> Fad.FxAlgorithmSetting -> m Fad.FxAlgorithmSetting
createRandomFxAlgorithmSetting reset andRate ix = do
  taAndR <- (\x -> truncate $ (andRate * fromIntegral x)) <$>
            getRandomR(max 1 (Fad.algorithmAndRate ix - Gsd.taMargin Gsd.gsd), 1 + Fad.algorithmAndRate ix + Gsd.taMargin Gsd.gsd)
  taOrR  <- getRandomR(max 1 (Fad.algorithmOrRate  ix - Gsd.taMargin Gsd.gsd), 1 + Fad.algorithmOrRate  ix + Gsd.taMargin Gsd.gsd)
  at <- if reset
        then Tr.makeTree taAndR taOrR (Fad.algorithmListCount ix) Tr.Empty
        else Tr.makeTree taAndR taOrR (Fad.algorithmListCount ix) (Fad.algorithmTree ix)
  sc <- getRandomR (max 1 (Fad.simChart ix - Gsd.taMargin Gsd.gsd), 1 + Fad.simChart ix + Gsd.taMargin Gsd.gsd)
  sma  <- createRandomFxAlMaSetting $ Fad.smaSetting  ix
  ema  <- createRandomFxAlMaSetting $ Fad.emaSetting  ix
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
              , Fad.macdSetting      = macd
              , Fad.stSetting        = st
              , Fad.rsiSetting       = rsi
              , Fad.simChart         = sc
              }

createRandomFxTechnicalAnalysisSetting :: MonadRandom m => Bool -> Double ->
                                          Fad.FxTechnicalAnalysisSetting -> m Fad.FxTechnicalAnalysisSetting
createRandomFxTechnicalAnalysisSetting reset andRate ix = do
  taAndR <- (\x -> truncate $ (andRate * fromIntegral x)) <$>
            getRandomR(max 1 (Fad.treeAnaAndRate ix - Gsd.taMargin Gsd.gsd), 1 + Fad.treeAnaAndRate ix + Gsd.taMargin Gsd.gsd)
  taOrR  <- getRandomR(max 1 (Fad.treeAnaOrRate  ix - Gsd.taMargin Gsd.gsd), 1 + Fad.treeAnaOrRate  ix + Gsd.taMargin Gsd.gsd)
  tat <- if reset
         then Tr.makeTree taAndR taOrR (Fad.techListCount ix) Tr.Empty
         else Tr.makeTree taAndR taOrR (Fad.techListCount ix) (Fad.techAnaTree ix)
  as' <- mapM (createRandomFxAlgorithmSetting reset andRate) $ Fad.algoSetting ix
  return $ ix { Fad.techAnaTree    = tat
              , Fad.algoSetting    = as'
              , Fad.treeAnaAndRate = taAndR
              , Fad.treeAnaOrRate  = taOrR
              }

createRandomGaData :: MonadRandom m => Bool -> Fsd.FxSettingData -> m (Ga.LearningData Fsd.FxSettingData)
createRandomGaData reset ix = do
  faso  <- createRandomFxTechnicalAnalysisSetting reset (Gsd.taOpenAndRate        Gsd.gsd) . Fsd.fxTaOpen        $ Fsd.fxSetting ix
  fascp <- createRandomFxTechnicalAnalysisSetting reset (Gsd.taCloseProfitAndRate Gsd.gsd) . Fsd.fxTaCloseProfit $ Fsd.fxSetting ix
  fascl <- createRandomFxTechnicalAnalysisSetting reset (Gsd.taCloseLossAndRate   Gsd.gsd) . Fsd.fxTaCloseLoss   $ Fsd.fxSetting ix
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
  Ga.learningDataList <$>
  concat <$>
  mapM (\_ -> mapM (createRandomGaData False) $ Ga.getGaDataList x) [1..n]

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
  resetFxSettingData x

resetFxSettingData :: MonadRandom m =>
                      Ga.LearningData Fsd.FxSettingData ->
                      m (Ga.LearningData  Fsd.FxSettingData)
resetFxSettingData x =
  createRandomGaData False $ Ga.getHeadGaData x

setHashFxSettingData :: Ga.LearningData Fsd.FxSettingData ->
                        Ga.LearningData Fsd.FxSettingData
setHashFxSettingData x =
  Ga.LearningData . map (\(fsd, p) -> (fsd { Fsd.fxSetting = (Fsd.fxSetting fsd)
                                             { Fsd.settingHash = hash (Fsd.fxSetting fsd)
                                             }
                                           }, p)) $ Ga.getLearningData x

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
  (macda, macdb) <- crossoverOrdFxAlMaSetting (Fad.macdSetting a) (Fad.macdSetting b)
  (rsia, rsib)   <- crossoverOrdFxAlMaSetting (Fad.rsiSetting a) (Fad.rsiSetting b)
  return ( a { Fad.algorithmTree    = ta
             , Fad.algorithmAndRate = choice1 die 0 (Fad.algorithmAndRate a) (Fad.algorithmAndRate b)
             , Fad.algorithmOrRate  = choice1 die 1 (Fad.algorithmOrRate  a) (Fad.algorithmOrRate  b)
             , Fad.rciSetting       = rcia
             , Fad.smaSetting       = smaa
             , Fad.emaSetting       = emaa
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
             , Fad.thresholdSetting = choice1 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
             }
         , b { Fad.shortSetting     = head b'
             , Fad.middleSetting    = b' !! 1
             , Fad.longSetting      = b' !! 2
             , Fad.thresholdSetting = choice2 die 1 (Fad.thresholdSetting a)  (Fad.thresholdSetting b)
            }
         )


