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
  getGaLoopMax :: a -> Int
  plusGaLoopMax :: a -> a
  reset :: MonadRandom m => LearningData a -> m (LearningData a)

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

evaluate :: (Ga a) => LearningData a -> LearningData a
evaluate (LearningData y) =
  LearningData . filter (\x -> 0 < snd x) . map (learningEvaluate . fst) . nub $ map (\x -> (fst x, 0 :: Rational)) y

selection :: MonadRandom m => LearningData a -> m (LearningData a)
selection x = do
  x' <- if (sum . map snd $ getLearningData x) == 0
        then fromList . map (\(f, _) -> (f, 1)) $ getLearningData x
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
         then return $ fmap plusGaLoopMax x'
         else if null x'
              then learningLoop (c + 1) glm x
              else learningLoop (c + 1) glm x'

createInitialDataLoop :: (Ga a, MonadRandom m) => Int -> Int -> LearningData a -> LearningData a -> m (LearningData a)
createInitialDataLoop c glm ix x = do
  x' <- mappend x . evaluate <$> createInitialData glm ix 
  traceShow("create", glm, c, length ix, length x, length x') $ return ()
  if glm < length x' `div` 2
    then return x'
    else if glm  < c
         then return $ fmap plusGaLoopMax x'
         else createInitialDataLoop (c + 1) glm ix x'

learning :: (Ga a, MonadRandom m) => LearningData a -> m (LearningData a)
learning x = do
  let glm = getGaLoopMax $ getHeadGaData x
  r <- reset x
  x' <- createInitialDataLoop 0 glm x emptyLearningData -- . evaluate $ mappend x r
  if null x'
    then return x
    else learningLoop 0 glm x'




