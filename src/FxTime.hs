module FxTime
  ( getLogTime
  , getDate
  ) where

import Data.Time

getLogTime :: IO String
getLogTime = take 19 . show <$> getZonedTime

getDate :: IO String
getDate = replace '-' '/' . take 10 . show <$> getZonedTime
  where
    replace :: (Eq a) => a -> a -> [a] -> [a]
    replace _ _ [] = []
    replace from to (x:xs) | from == x = to : (replace from to xs)
                           | otherwise = x  : (replace from to xs)
