module Ga
  ( LearningData (..)
  , Ga (..)
  ) where

import           Control.Monad.Random
import qualified Data.Foldable        as F
import           Data.List
import           Debug.Trace

newtype LearningData a = LearningData { getLearningData :: [(a, Rational)] } deriving (Show, Eq, Ord)

instance Semigroup (LearningData a) where
  LearningData x <> LearningData y = LearningData $ x ++ y

instance Monoid (LearningData a) where
  mempty = LearningData []
  LearningData x `mappend` LearningData y = LearningData $ x ++ y

instance F.Foldable LearningData where
  foldMap f (LearningData x) = F.foldMap f $ map fst x
  maximum (LearningData x) = snd . maximum $ map (\y -> (snd y, fst y)) x

instance Functor LearningData where
  fmap f (LearningData x) = LearningData $ zip (map (f . fst) x) (map snd x)

class (Show a, Eq a, Ord a) => Ga a where
  copy :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  mutation :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  crossover :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  createInitialData :: MonadRandom m => Int -> LearningData a -> m (LearningData a)
  learningEvaluate :: Int -> (a, Rational) -> IO (a, Rational)
  setHash :: LearningData a -> LearningData a
  getGaDataList :: LearningData a -> [a]
  maximumScore :: LearningData a -> Rational
  getHeadGaData :: LearningData a -> a
  emptyLearningData :: LearningData a
  learningData :: a -> LearningData a
  learningDataList :: [LearningData a] -> LearningData a
  learning :: Int -> Int -> LearningData a -> IO (LearningData a)
  
  getGaDataList (LearningData x) = map fst x
  maximumScore (LearningData x) = fst . maximum $ map (\y -> (snd y, fst y)) x
  getHeadGaData (LearningData x) = fst $ head x
  emptyLearningData = LearningData []
  learningData x = LearningData [(x, 0)]
  learningDataList s = LearningData . foldl1 (++) $ map (\(LearningData x) -> x) s

  learning n m x = do
    (ok, x') <- createLoop n (length x + 10) m 0 x emptyLearningData
    if ok
      then gaLoop n (length x + 10) x'
      else return $ setHash x'

selection :: (Ga a, MonadRandom m) => LearningData a -> m (LearningData a)
selection x = do
  let mp = minimum . map snd $ getLearningData x
  x' <- if mp <= 0
        then fromList . map (\(f, p) -> (f, 1 + p + abs mp)) $ getLearningData x
        else fromList $ getLearningData x
  return $ learningData x'

selection2 :: (Ga a, MonadRandom m) => LearningData a -> m (LearningData a, LearningData a)
selection2 x = do
  s1 <- selection x
  s2 <- selection x
  return (s1, s2)

selectAlgorithm :: (Ga a, MonadRandom m) => m (LearningData a -> LearningData a -> m (LearningData a))
selectAlgorithm = do
  die <- getRandomR (1, 100)
  let x | 95 < (die :: Int) = copy
        | die <= 5 = mutation
        | otherwise = crossover
  return x

geneticOperators :: (Ga a, MonadRandom m) => Int -> LearningData a -> LearningData a -> m (LearningData a)
geneticOperators e x y = do
  (s1, s2) <- selection2 x
  af <- selectAlgorithm
  y' <- mappend y <$> af s1 s2
  if e <= length y'
    then return y'
    else geneticOperators e x y'

createLoop :: (Ga a) => Int -> Int -> Int -> Int -> LearningData a -> LearningData a -> IO (Bool, LearningData a)
createLoop n e m c x y = do
  x' <- createInitialData e x
  y' <- mappend y <$> createEvaluate n x'
  -- traceShow("create", e, m, c, length y', length x, length y) $ return ()
  if e <= length y'
    then return (True, y')
    else if m < c
         then if 2 < length y'
              then return (True, y')
              else return (False, x)
         else createLoop n e m (c + 1) x y'

gaLoop :: (Ga a) => Int -> Int -> LearningData a -> IO (LearningData a)
gaLoop n e x = do
  x' <- gaEvaluate n =<< (geneticOperators e x $ LearningData [(maximum x, maximumScore x)])
  -- traceShow("ga", length x, length x', fromRational $ maximumScore x', fromRational $ maximumScore x) $ return ()
  if maximumScore x' == maximumScore x
    then return x'
    else gaLoop n e $ mappend x' x

createEvaluate :: (Ga a) => Int -> LearningData a -> IO (LearningData a)
createEvaluate n (LearningData y) = do
  LearningData <$> (mapM (learningEvaluate n) $ map (\x -> (fst x, 0 :: Rational)) y)

gaEvaluate :: (Ga a) => Int -> LearningData a -> IO (LearningData a)
gaEvaluate n (LearningData y) = do
  LearningData <$> mapM (learningEvaluate n) y
    
              

    





