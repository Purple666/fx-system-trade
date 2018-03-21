module Ga
  ( LearningData (..)
  , Ga (..)
  , evaluate
  , learning
  , emptyLearningData
  , getGaDataList 
  , getHeadGaData
  , learningData
  , learningDataList
  ) where

import qualified Data.Foldable           as F
import Data.List
import Control.Monad.Random
import Debug.Trace 

newtype LearningData a = LearningData { getLearningData :: [(a, Rational)] } deriving (Show, Eq, Ord)

instance Monoid (LearningData a) where
  mempty = LearningData []
  LearningData x `mappend` LearningData y = LearningData $ x ++ y
  
instance F.Foldable LearningData where
  foldMap f (LearningData x) = F.foldMap f $ map (fst) x
  maximum (LearningData x) = snd . maximum $ map (\y -> (snd y, fst y)) x
  
instance Functor LearningData where
  fmap f (LearningData x) = LearningData $ zip (map f $ map fst x) (map snd x)


class (Show a, Eq a, Ord a) => Ga a where
  reset :: MonadRandom m => a -> m a
  copy :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  mutation :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  crossover :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  createInitialData :: MonadRandom m => Int -> a -> m (LearningData a)
  learningEvaluate :: a -> (a, Rational)
  getGaLength :: a -> Int
  getGaLoopMax :: a -> Int
  plusGaLoopMax :: a -> a

getGaDataList :: LearningData a -> [a]
getGaDataList (LearningData x) = map fst x

getHeadGaData :: LearningData a -> a
getHeadGaData (LearningData x) = fst $ head x

emptyLearningData :: LearningData a
emptyLearningData = LearningData []

learningData :: a -> LearningData a
learningData s = LearningData [(s, 0)]

learningDataList :: [LearningData a] -> LearningData a
learningDataList s = LearningData . foldl1 (\acc x -> acc ++ x) $ map (\(LearningData x) -> x) s

evaluate :: (Ga a) => LearningData a -> LearningData a
evaluate (LearningData y) = do
  LearningData . map (\x -> learningEvaluate $ fst x) . nub $ map (\x -> (fst x, (0 :: Rational))) y

selection :: MonadRandom m => LearningData a -> m (LearningData a)
selection x = do
  x' <- fromList $ getLearningData x
  return $ learningData x'
    
selection2 :: (Ga a, MonadRandom m) => LearningData a -> m (LearningData a, LearningData a)
selection2 x = do
  s1 <- selection x
  s2 <- selection x
  return (s1, s2)

selectAlgorithm :: (Ga a, MonadRandom m) => m (LearningData a -> LearningData a -> m (LearningData a))
selectAlgorithm = do
  die <- getRandomR (1, 100)
  let x = if 95 < (die :: Int)
          then (\a b -> copy a b)
          else if die <= 5
               then (\a b -> mutation a b)
               else (\a b -> crossover a b)
  return x

geneticOperators :: (Ga a, MonadRandom m) => Int -> LearningData a -> LearningData a -> m (LearningData a)
geneticOperators e x y = do
  (s1, s2) <- selection2 x
  algorithmFunftion <- selectAlgorithm
  y' <- mappend y <$> algorithmFunftion s1 s2
  if e <= length y'
    then return (y')
    else geneticOperators e x y'

learningLoop :: (Ga a, MonadRandom m) =>
                Int -> a -> LearningData a -> m (LearningData a)
learningLoop c ix x = do
  x' <- evaluate <$> (geneticOperators (getGaLength ix) x . learningData $ maximum x)
  --traceShow("loop", c, length x, length x') $ return ()
  if maximum x' == maximum x
    then return (x')
    else if getGaLoopMax ix < c 
         then return (fmap plusGaLoopMax x')
    else learningLoop (c + 1) ix x'

learning :: (Ga a, MonadRandom m) => a -> m (LearningData a)
learning ix = do
  learningLoop 0 ix =<< (evaluate <$> createInitialData (getGaLength ix) ix)
  

