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

import           Control.Monad.Random
import qualified Data.Foldable        as F
import           Data.List
-- import           Debug.Trace

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
  reset :: MonadRandom m => a -> m a
  copy :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  mutation :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  crossover :: MonadRandom m => LearningData a -> LearningData a -> m (LearningData a)
  createInitialData :: MonadRandom m => Int -> a -> m (LearningData a)
  learningEvaluate :: a -> (a, Rational)
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
learningDataList s = LearningData . foldl1 (++) $ map (\(LearningData x) -> x) s

top :: (Ga a) => Int -> LearningData a -> LearningData a
top n (LearningData y) =
  LearningData . take n $ sortBy(\(_, a) (_, b) -> compare b a) y

evaluate :: (Ga a) => LearningData a -> LearningData a
evaluate (LearningData y) =
  LearningData . filter (\x -> 0 < snd x) . map (learningEvaluate . fst) . nub $ map (\x -> (fst x, 0 :: Rational)) y

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
  let x | 95 < (die :: Int) = copy
        | die <= 5 = mutation
        | otherwise = crossover
  return x

geneticOperators :: (Ga a, MonadRandom m) => Int -> LearningData a -> LearningData a -> m (LearningData a)
geneticOperators e x y = do
  (s1, s2) <- selection2 x
  algorithmFunftion <- selectAlgorithm
  y' <- mappend y <$> algorithmFunftion s1 s2
  if e < length y'
    then return y'
    else geneticOperators e x y'

learningLoop :: (Ga a, MonadRandom m) =>
                Int -> Int -> LearningData a -> m (LearningData a)
learningLoop c glm x = do
  x' <- evaluate <$> (geneticOperators glm x . learningData $ maximum x)
  traceShow("ga", glm, c, length x, length x') $ return ()
  if not (null x') && not (null x) && maximum x' == maximum x
    then return x'
    else if glm < c
         then return $ fmap plusGaLoopMax x
         else if null x'
              then learningLoop (c + 1) glm x
              else learningLoop (c + 1) glm x'

createInitialDataLoop :: (Ga a, MonadRandom m) => Int -> Int -> [a] -> LearningData a -> m (LearningData a)
createInitialDataLoop c glm ixs x = do
  x' <- mappend x . evaluate . learningDataList <$> mapM (createInitialData glm) ixs
  traceShow("create", glm, c, length ixs, length x, length x') $ return ()
  if glm < length x'
    then return x'
    else if glm  < c
         then return $ fmap plusGaLoopMax x'
         else if null x'
              then createInitialDataLoop (c + 1) glm ixs x'
              else createInitialDataLoop (c + 1) glm (getGaDataList x') x'

learning :: (Ga a, MonadRandom m) => a -> [a] -> m (LearningData a)
learning ix ixs = do
  ix' <- reset ix
  let x = learningDataList . map learningData $ ix':ix:ixs
      glm = getGaLoopMax ix
  x' <- top glm <$> createInitialDataLoop 0 glm (ix':ix:ixs) (evaluate x)
  if null x'
    then return x
    else learningLoop 0 glm (length x') x'





