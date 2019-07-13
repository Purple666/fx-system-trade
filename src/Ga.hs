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
  learningEvaluate :: a -> (a, Rational)
  setHash :: LearningData a -> LearningData a
  getGaDataList :: LearningData a -> [a]
  maximumScore :: LearningData a -> Rational
  getHeadGaData :: LearningData a -> a
  emptyLearningData :: LearningData a
  learningData :: a -> LearningData a
  learningDataList :: [LearningData a] -> LearningData a
  evaluate :: LearningData a -> LearningData a
  learning :: MonadRandom m => LearningData a -> m (LearningData a)
  
  getGaDataList (LearningData x) = map fst x
  maximumScore (LearningData x) = fst . maximum $ map (\y -> (snd y, fst y)) x
  getHeadGaData (LearningData x) = fst $ head x
  emptyLearningData = LearningData []
  learningData x = LearningData [(x, 0)]
  learningDataList s = LearningData . foldl1 (++) $ map (\(LearningData x) -> x) s
  evaluate (LearningData y) = LearningData . filter(\(_, p) -> 0 < p) . map (learningEvaluate . fst) $ map (\x -> (fst x, 0 :: Rational)) y
  learning x = do setHash <$> (gaLoop (length x + 2) =<< createLoop (length x + 2) x emptyLearningData)

selection :: (Ga a, MonadRandom m) => LearningData a -> m (LearningData a)
selection x = do
  learningData <$> (fromList $ getLearningData x)

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

createLoop :: (Ga a, MonadRandom m) => Int -> LearningData a -> LearningData a -> m (LearningData a)
createLoop e x y = do
  x' <- createInitialData e x
  let y' = mappend y $ evaluate x'
  traceShow("create", e, length y', length x, length y) $ return ()
  if e <= length y'
    then return . LearningData . take e . sortBy (\(_, a) (_, b) -> compare b a) $ getLearningData y'
    else createLoop e (x `mappend` x') y'

gaLoop :: (Ga a, MonadRandom m) =>
                Int -> LearningData a -> m (LearningData a)
gaLoop e x = do
  x' <- evaluate <$> (geneticOperators e x . learningData $ maximum x)
  traceShow("ga", length x, length x', fromRational $ maximumScore x', fromRational $ maximumScore x) $ return ()
  if maximumScore x' == maximumScore x
    then return x'
    else gaLoop e x'




