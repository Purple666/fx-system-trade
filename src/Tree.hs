{-# LANGUAGE DeriveGeneric #-}

module Tree
  ( LeafData (..)
  , NodeData (..)
  , LeafDataMap (..)
  , TreeData (..)
  , evaluateTree
  , makeTree
  , crossoverTree
  , adjustTree
  , setFunctionToLeafDataMap
  , setFunctionToTree
  , makeValidLeafDataList
  , calcValidLeafDataList
  , addLeafDataMap
  , checkLeafDataMap
  , emptyLeafDataMap
  , initLeafDataMap
  ) where

import qualified Control.Monad.Random as R
import           Data.List
import qualified Data.Map             as M
import qualified GlobalSettingData    as Gsd
import GHC.Generics (Generic)
import Data.Hashable
import Debug.Trace

newtype LeafData a = LeafData { getLeafData :: (Int, (a -> Bool, a -> Bool)) } deriving(Generic)

newtype NodeData = NodeData { getNodeData :: (Int, Bool -> Bool -> Bool) } deriving(Generic)

newtype LeafDataMap a = LeafDataMap { getLeafDataMap :: M.Map (LeafData a) (Double, Int) } deriving(Show, Read, Eq, Ord, Generic)

instance Hashable a => Hashable (LeafDataMap a)

instance Hashable a => Hashable (LeafData a) where
   hashWithSalt = hashUsing (fst . getLeafData)

instance Hashable NodeData where
   hashWithSalt = hashUsing (fst . getNodeData)

instance (Hashable k, Hashable a) => Hashable (M.Map k a) where
   hashWithSalt = hashUsing (hash . M.toList)

instance Read (LeafData a) where
  readsPrec _ s = let (a, s') = break (\x -> x ==')' || x ==',' || x ==']' ) s
                  in [(LeafData (read a, (defaultFunction, defaultFunction)), s')]

instance Read NodeData where
  readsPrec _ s = let a = read $ take 2 s
                      s' = drop 2 s
                  in if a == 0
                     then [(NodeData (a, (&&)), s')]
                     else [(NodeData (a, (||)), s')]

instance Show (LeafData a) where
  show a = show . fst $ getLeafData a

instance Show NodeData where
  show a = show . fst $ getNodeData a

instance Eq (LeafData a) where
  a == b = fst (getLeafData a) == fst (getLeafData b)

instance Eq NodeData where  
  a == b = fst (getNodeData a) == fst (getNodeData b)

instance Ord (LeafData a) where
  compare (LeafData a) (LeafData b) = compare (fst a) (fst b)

instance Ord NodeData where
  compare (NodeData a) (NodeData b) = compare (fst a) (fst b)

data TreeData a = Empty  |
                  Leaf (LeafData a) |
                  Node NodeData (TreeData a) (TreeData a) deriving(Show, Read, Eq, Ord, Generic)

instance Hashable a => Hashable (TreeData a)

defaultFunction :: a -> Bool
defaultFunction _ = True

emptyLeafDataMap :: LeafDataMap a
emptyLeafDataMap = LeafDataMap M.empty

initLeafDataMap :: LeafData a -> LeafDataMap a
initLeafDataMap k = LeafDataMap $ M.singleton k (0, 0)

setFunctionToLeafDataMap :: [LeafData a] -> LeafDataMap a -> LeafDataMap a
setFunctionToLeafDataMap ix (LeafDataMap xs) =
  LeafDataMap . M.fromList . map (\(LeafData k, (x, y)) -> (ix !! fst k, (x, y))) $ M.toList xs

setFunctionToTree :: [LeafData a] -> TreeData a -> TreeData a
setFunctionToTree ix (Leaf (LeafData (k, (_, _)))) = Leaf (ix !! k)
setFunctionToTree _ Empty = Empty
setFunctionToTree ix (Node x l r) = Node x (setFunctionToTree ix l) (setFunctionToTree ix r)

checkLeafDataMap :: LeafDataMap a -> (LeafDataMap a, LeafDataMap a)
checkLeafDataMap (LeafDataMap xs) =
  let xs' = if (fst $ minimum xs) < 0
            then M.map (\(x, y) -> (x + (abs . fst $ minimum xs) + 1, y)) xs
            else xs
  in if (fst $ minimum xs') * (Gsd.countUpList $ Gsd.gsd) < (fst $ maximum xs')
     then let (a, b) = M.partition (\(x, _) -> (fst $ minimum xs') * (Gsd.countUpList $ Gsd.gsd) < x) xs'
          in (LeafDataMap a, LeafDataMap b)
     else (LeafDataMap M.empty, LeafDataMap xs')

makeTree :: R.MonadRandom m => Int -> Int -> LeafDataMap a -> TreeData a -> m (TreeData a)
makeTree andRate orRate (LeafDataMap xs) t =
  if null xs
    then return t
    else do let mx = fst . minimum . map snd $ M.toList xs
                xs' = if mx <= 0
                      then M.map (\(x, _) -> toRational (x + abs mx + 1)) xs
                      else M.map (toRational . fst) xs
            foldl (\acc _ -> do l <- R.fromList $ M.toList xs'
                                insertTree andRate orRate (Leaf l) =<< acc
                  ) (pure t) [0..Gsd.makeTreeCount Gsd.gsd]

adjustTree :: LeafDataMap a -> TreeData a -> TreeData a
adjustTree _ Empty = Empty
adjustTree (LeafDataMap dm) (Leaf x) =
  if M.member x dm
  then Leaf x
  else Empty
adjustTree e (Node x l r) = Node x (adjustTree e l) (adjustTree e r)

insertTree :: R.MonadRandom m => Int -> Int -> TreeData a -> TreeData a -> m (TreeData a)
insertTree _ _ e Empty = return e
insertTree andRate orRate e (Leaf x) = do
  die <- R.fromList [(NodeData (0, (&&)), toRational andRate), (NodeData (1, (||)), toRational orRate)]
  return (Node die e (Leaf x))
insertTree _ _ e (Node x l Empty) =
  if e == l
  then return (Node x l Empty)
  else return (Node x l e)
insertTree _ _ e (Node x Empty r) =
  if e == r
  then return (Node x Empty r)
  else return (Node x e r)
insertTree andRate orRate e (Node x l r) =
  if e == l || e == r
    then return (Node x l r)
    else do die <- R.getRandomR (True, False)
            if die
              then do l' <- insertTree andRate orRate e l
                      return (Node x l' r)
              else do r' <- insertTree andRate orRate e r
                      return (Node x l r')

crossoverTree :: R.MonadRandom m =>
                 Int -> Int ->
                 TreeData a -> LeafDataMap a -> TreeData a -> LeafDataMap a -> m (TreeData a, TreeData a)
crossoverTree andRate orRate x xdm y ydm = do
  (xo, xd) <- divideTree x
  (yo, yd) <- divideTree y
  x' <- insertTree andRate orRate xo yd
  y' <- insertTree andRate orRate yo xd
  return (adjustTree xdm x', adjustTree ydm y')

divideTree :: R.MonadRandom m => TreeData a -> m (TreeData a, TreeData a)
divideTree Empty = return (Empty, Empty)
divideTree (Leaf x) = do
  die <- R.getRandomR (True, False)
  if die
    then return (Leaf x, Empty)
    else return (Empty, Leaf x)
divideTree (Node x l r) = do
  die <- R.getRandomR (True, False)
  if die
    then do die2 <- R.getRandomR (True, False)
            if die
              then return (r, l)
              else return (l, r)
    else do die2 <- R.getRandomR (True, False)
            if die2
              then do (o, d) <- divideTree l
                      die3 <- R.getRandomR (True, False)
                      if die3
                        then return (Node x r o, d)
                        else return (o, Node x r d)
              else do (o, d) <- divideTree r
                      if die
                        then return (Node x l o, d)
                        else return (o, Node x l d)

evaluateTree :: ((a -> Bool, a -> Bool) -> (a -> Bool)) -> a -> TreeData a -> Bool
evaluateTree f s (Leaf x) = (f . snd $ getLeafData x) s
evaluateTree _ _ Empty = False
evaluateTree f s (Node _ l Empty) = evaluateTree f s l
evaluateTree f s (Node _ Empty r) = evaluateTree f s r
evaluateTree f s (Node x l r) = (snd $ getNodeData x) (evaluateTree f s l) (evaluateTree f s r)

addLeafDataMap :: LeafDataMap a -> LeafDataMap a -> LeafDataMap a
addLeafDataMap (LeafDataMap a) (LeafDataMap b) =
  LeafDataMap $ M.unionWith (\(x, y) (x', y') -> (x + x', y + y')) a b

calcValidLeafDataList :: Double -> [LeafData a] -> LeafDataMap a
calcValidLeafDataList p lds =
  foldl (\acc k -> addLeafDataMap (LeafDataMap $ M.singleton k (p, 1)) acc) emptyLeafDataMap lds

makeValidLeafDataList :: ((a -> Bool, a -> Bool) -> (a -> Bool)) -> a -> TreeData a -> [LeafData a]
makeValidLeafDataList f s tl =
  let a = evaluateTrueLeafDataList f s tl
  in traceShow(a) $ nub a

evaluateTrueLeafDataList :: ((a -> Bool, a -> Bool) -> (a -> Bool)) -> a -> TreeData a -> [LeafData a]
evaluateTrueLeafDataList _ _ Empty = []
evaluateTrueLeafDataList f s (Node _ l Empty) = evaluateTrueLeafDataList f s l
evaluateTrueLeafDataList f s (Node _ Empty r) = evaluateTrueLeafDataList f s r
evaluateTrueLeafDataList f s (Leaf x) =
  [x | (f . snd $ getLeafData x) s]
evaluateTrueLeafDataList f s (Node x l r) =
  let l' = evaluateTrueLeafDataList f s l
      r' = evaluateTrueLeafDataList f s r
  in case fst $ getNodeData x of
    0 -> if null l' || null r'
         then []
         else l' ++ r'
    _ -> l' ++ r'

