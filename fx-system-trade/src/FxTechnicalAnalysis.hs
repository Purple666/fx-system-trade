module FxTechnicalAnalysis
  ( makeFxTechnicalAnalysisDataList
  , makeValidLeafDataMapInc
  , makeValidLeafDataMapDec
  , addFxalgorithmListCount
  , getPrepareTime
  , updateAlgorithmListCount
  , setFxTechnicalAnalysisSetting
  , getSimChartMax
  ) where

import           Data.List
import qualified Data.Map                as M
--import           Debug.Trace
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

getSimChartMax :: Fad.FxTechnicalAnalysisSetting -> Int
getSimChartMax x =
  maximum $ M.map (\a -> let prevSettingMax = maximum
                               [ Fad.prevSetting $ Fad.smaSetting a
                               , Fad.prevSetting $ Fad.emaSetting a
                               , Fad.prevSetting $ Fad.wmaSetting a
                               , Fad.prevSetting $ Fad.macdSetting a
                               , Fad.prevSetting $ Fad.stSetting a
                               , Fad.prevSetting $ Fad.rciSetting a
                               , Fad.prevSetting $ Fad.rsiSetting a
                               ]
                         in {- prevSettingMax * -} Fad.simChart a) $ Fad.algoSetting x

checkAlgoSetting :: M.Map Int Fad.FxAlgorithmSetting ->
                    Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData) ->
                    (M.Map Int Fad.FxAlgorithmSetting,
                     Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData))
checkAlgoSetting as tlc =
  let (as', pr) = foldl (\(acc, p) k -> let x = as M.! k
                                            (a, b) = Tr.checkLeafDataMap $ Fad.algorithmListCount x
                                            x' = x { Fad.algorithmListCount = Tr.addLeafDataMap b p
                                                   , Fad.algorithmTree = Tr.adjustTree (Fad.algorithmListCount x') (Fad.algorithmTree x)
                                                   }
                                        in (M.adjust (const x') k acc, a)) (as, Tr.emptyLeafDataMap)
                  . sort $ M.keys as
  in if not . M.null $ Tr.getLeafDataMap pr
     then let nk = fst (M.findMax as) + 1
          in (M.insert nk (Fad.initFxAlgorithmSetting pr) as',
              Tr.LeafDataMap . M.insert (Fad.initTechAnaLeafData nk) 1 $ Tr.getLeafDataMap tlc)
     else (as', tlc)

updateAlgorithmListCount :: (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) -> [Fad.FxChartTaData] ->
                            (Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData),
                             M.Map Int (Tr.LeafDataMap Fad.FxTechnicalAnalysisData)) ->
                            Fad.FxTechnicalAnalysisSetting -> Fad.FxTechnicalAnalysisSetting
updateAlgorithmListCount f ctdl (ldlt, ldla) fts =
  let fts' = fts { Fad.techListCount = Tr.addLeafDataMap (Fad.techListCount fts) ldlt
                 , Fad.algoSetting   = M.foldrWithKey (\k x acc -> let y = acc M.! k
                                                                       y' = y { Fad.algorithmListCount =
                                                                                Tr.addLeafDataMap x (Fad.algorithmListCount y) }
                                                                    in M.union (M.singleton k y') acc) (Fad.algoSetting fts) ldla
                 }
      as = updateThreshold f ctdl (Fad.algoSetting fts')
      (as', tlc) = checkAlgoSetting as (Fad.techListCount fts')
  in fts' { Fad.techListCount = tlc
          , Fad.algoSetting   = as'
          }

makeValidLeafDataMapInc :: Fad.FxTechnicalAnalysisSetting ->
                           M.Map Int Fad.FxTechnicalAnalysisData ->
                           ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData])
makeValidLeafDataMapInc fts ftad =
  let l = Tr.makeValidLeafDataList fst (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)
  in (l, M.fromList $ map (\x -> let n = fst $ Tr.getLeafData x
                                 in (n, Tr.makeValidLeafDataList fst (ftad M.! n) (Fad.algorithmTree $ Fad.algoSetting fts M.! n))) l)

makeValidLeafDataMapDec :: Fad.FxTechnicalAnalysisSetting ->
                           M.Map Int Fad.FxTechnicalAnalysisData ->
                           ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData])
makeValidLeafDataMapDec fts ftad =
  let l = Tr.makeValidLeafDataList snd (Fad.algoSetting fts, ftad) (Fad.techAnaTree fts)
  in (l, M.fromList $ map (\x -> let n = fst $ Tr.getLeafData x
                                 in (n, Tr.makeValidLeafDataList snd (ftad M.! n) (Fad.algorithmTree $ Fad.algoSetting fts M.! n))) l)

addFxalgorithmListCount :: Double ->
                           ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData]) ->
                           (Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData),
                            M.Map Int (Tr.LeafDataMap Fad.FxTechnicalAnalysisData)) ->
                           (Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData),
                            M.Map Int (Tr.LeafDataMap Fad.FxTechnicalAnalysisData))
addFxalgorithmListCount p (ptat, pat) (tat, at) =
  (Tr.addValidLeafDataList p ptat tat,
    M.union (M.mapWithKey (\k x -> if M.member k at
                                   then Tr.addValidLeafDataList p x (at M.! k)
                                   else Tr.addValidLeafDataList p x $ M.insert k Tr.emptyLeafDataMap at M.! k) pat) at)


getThreshold :: Int ->
                [Fad.FxChartTaData] ->
                (Fad.FxTechnicalAnalysisData -> Fad.FxMovingAverageData) ->
                (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) ->
                Double
getThreshold k ctdl f1 f2 =
  let l =  sort $ foldl (\acc x -> (Fad.short  . f1 $ f2 x M.! k):
                                   (Fad.middle . f1 $ f2 x M.! k):
                                   (Fad.long   . f1 $ f2 x M.! k):acc) [] ctdl
  in last $ take (truncate $ fromIntegral (length l) * Gsd.thresholdRate Gsd.gsd) l

updateThreshold :: (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) ->
                   [Fad.FxChartTaData] -> M.Map Int Fad.FxAlgorithmSetting -> M.Map Int Fad.FxAlgorithmSetting
updateThreshold f ctdl =
  M.mapWithKey (\k x -> x { Fad.stSetting  = (Fad.stSetting x)
                            { Fad.thresholdMaxSetting = (getThreshold k ctdl Fad.st f + Fad.thresholdMaxSetting (Fad.stSetting x)) / 2
                            }
                          , Fad.rciSetting = (Fad.rciSetting x)
                            { Fad.thresholdMaxSetting = (100 + getThreshold k ctdl Fad.rci f + Fad.thresholdMaxSetting (Fad.rciSetting x)) / 2
                            }
                          , Fad.rsiSetting = (Fad.rsiSetting x)
                            { Fad.thresholdMaxSetting = (getThreshold k ctdl Fad.rsi f + Fad.thresholdMaxSetting (Fad.rsiSetting x)) / 2
                            }
                          })

getPrepareTime :: Fad.FxTechnicalAnalysisSetting -> Int
getPrepareTime x =
  maximum $ M.map (\a -> maximum [ Fad.longSetting (Fad.rciSetting a) + Gsd.taMargin Gsd.gsd
                                 , Fad.longSetting (Fad.smaSetting a) + Gsd.taMargin Gsd.gsd
                                 , Fad.longSetting (Fad.emaSetting a) + Gsd.taMargin Gsd.gsd
                                 , Fad.longSetting (Fad.wmaSetting a) + Gsd.taMargin Gsd.gsd
                                 , Fad.longSetting (Fad.rsiSetting a) + Gsd.taMargin Gsd.gsd
                                 , Fad.longSetting (Fad.stSetting a)  + Gsd.taMargin Gsd.gsd
                                 ] * (Fad.simChart a + Gsd.taMargin Gsd.gsd)) $ Fad.algoSetting x

setFxTechnicalAnalysisSetting :: Fad.FxTechnicalAnalysisSetting -> Fad.FxTechnicalAnalysisSetting
setFxTechnicalAnalysisSetting x =
  let mk = maximum . M.keys $ Fad.algoSetting x
      itad = map Fad.initTechAnaLeafData [0..mk]
  in x { Fad.techAnaTree   = Tr.setFunctionToTree        itad $ Fad.techAnaTree x
       , Fad.techListCount = Tr.setFunctionToLeafDataMap itad $ Fad.techListCount x
       , Fad.algoSetting   = M.map setFxAlgorithmSetting $ Fad.algoSetting x
       }

setFxAlgorithmSetting :: Fad.FxAlgorithmSetting -> Fad.FxAlgorithmSetting
setFxAlgorithmSetting x =
  x { Fad.algorithmTree      = Tr.setFunctionToTree        Fad.initAlgoLeafData $ Fad.algorithmTree   x
    , Fad.algorithmListCount = Tr.setFunctionToLeafDataMap Fad.initAlgoLeafData $ Fad.algorithmListCount x
    }


rci :: Int -> [Double] -> Double
rci n x  =
  let r  = [1..n] :: [Int]
      r' = reverse [1..n] :: [Int]
      d = sum . map (\(a, b) -> (a - b) ^ (2 :: Int)) . zipWith (\a (_, b') -> (a, b')) r' . sort $ zip x r
  in (1 - (6.0 * fromIntegral d) / (fromIntegral n * (fromIntegral n ^ (2 :: Int) - 1))) * 100

lsm :: Int -> [Double] -> Double
lsm n y =
  let x = reverse $ take n [1..]
  in (fromIntegral n * sum (zipWith (*) x y) - sum x * sum y) / (fromIntegral n * sum (map (^(2 :: Int)) x) - sum x ^ (2 :: Int))

getRci :: Int -> [Fcd.FxChartData] -> Double
getRci n x =
  let s = take n $ map Fcd.close x
  in if length s < n
     then 0
     else rci n s

rsiUpDown :: Double ->  [Double] -> (Double, Double)
rsiUpDown _ []     = (0, 0)
rsiUpDown p [x] =
  if p < x
  then (x - p, 0)
  else (0, p - x)
rsiUpDown p (x:xs) =
  let (u, d) = rsiUpDown x xs
  in if p < x
     then ((x - p) + u, d)
     else (u, (p - x) + d)

rsi :: Int -> [Double] -> Double
rsi n x =
  let (up, down) = rsiUpDown (last x) (tail $ reverse x)
      upa = up / fromIntegral n
      downa = down / fromIntegral n
  in (100 * upa) / (upa + downa)

getRsi :: Int -> [Fcd.FxChartData] -> Double
getRsi n x =
  let s = take (n + 1) $ map Fcd.close x
  in if length s < n + 1
     then 50
     else rsi n s

getSma :: Int -> [Fcd.FxChartData] -> Double
getSma n x =
  let s = take n $ map Fcd.close x
  in if length s < n
     then 0
     else sum s / fromIntegral n

getEma :: Int -> [Fcd.FxChartData] -> Double
getEma n x =
  let s = take n $ map Fcd.close x
      h = Fcd.close $ head x
  in if length s < n
     then 0
     else (sum s + h) / (fromIntegral n + 1)

getWma :: Int -> [Fcd.FxChartData] -> Double
getWma n x =
  let s = take n $ map Fcd.close x
      r = reverse [1..n]
  in if length s < n
     then 0
     else (sum . map (\(a, b) -> a * fromIntegral b) $ zip s r) / fromIntegral (sum r)

getMACD :: Double -> Double -> Int -> [Fad.FxTechnicalAnalysisData] -> (Double, Double)
getMACD es el n x =
  let macd = if es ==0 || el == 0
             then 0
             else es - el
      signal = let s = take (n - 1) x
               in if length s < (n - 1) || macd == 0
                  then 0
                  else (sum (map (Fad.short . Fad.macd) s) + macd) / fromIntegral n
  in (macd, signal)

getBB :: Int -> Double -> [Fcd.FxChartData] -> Fad.FxMovingAverageData
getBB n ma x =
  let chart = head x
      s = take n $ map Fcd.close x
      sd = sqrt $ (fromIntegral n * foldl (\acc b -> (b ^ (2 :: Int) + acc)) 0 s - sum s ^ (2 :: Int)) / fromIntegral (n * (n - 1))
  in if length s < n || ma == 0
     then Fad.initFxMovingAverageData
     else Fad.initFxMovingAverageData { Fad.thresholdS = if Fcd.close chart < ma - sd * 3 ||
                                                            (ma + sd * 2 < Fcd.close chart && Fcd.close chart < ma + sd * 3)
                                                         then Fad.Buy
                                                         else if ma + sd * 3 < Fcd.close chart ||
                                                                 (ma - sd * 3 < Fcd.close chart && Fcd.close chart < ma - sd * 2)
                                                              then Fad.Sell
                                                              else Fad.None
                                      }

getST :: Int -> Int -> [Fcd.FxChartData] -> [Fad.FxTechnicalAnalysisData] -> (Double, Double, Double)
getST n m x p =
  let s1 = take n $ map Fcd.close x                                   -- setting long
      s2 = take m p                                                   -- setting short
      k  = ((head s1 - minimum s1) * 100 / (maximum s1 - minimum s1)) -- short
      d  = sum (map (Fad.short  . Fad.st) s2) / fromIntegral m        -- middle
      sd = sum (map (Fad.middle . Fad.st) s2) / fromIntegral m        -- long
  in if length s1 < n || length s2 < m
     then (50, 50, 50)
     else (k, d, sd)

setCross :: Double ->
            Double ->
            Double ->
            Double ->
            Fad.FxTradePosition
setCross s l ps pl
  | ps < pl && l < s = Fad.Buy
  | pl < ps && s < l = Fad.Sell
  | otherwise = Fad.None

lsmn :: [Double] -> Fad.FxTradePosition
lsmn xs
  | fst $ foldl (\(f, p) x -> (f && p <= x, p)) (True, 0) xs = Fad.Buy
  | fst $ foldl (\(f, p) x -> (f && x <= p, p)) (True, 0) xs = Fad.Sell
  | otherwise = Fad.None

setThreshold :: Double ->
                Double ->
                Double ->
                Fad.FxAlMaSetting ->
                Fad.FxTradePosition
setThreshold x tmin tmax ftms
  | x < tmin + Fad.thresholdSetting ftms = Fad.Buy
  | tmax - Fad.thresholdSetting ftms < x = Fad.Sell
  | otherwise = Fad.None

setFxMovingAverageData :: Double ->
                          Double ->
                          Double ->
                          Double ->
                          Double ->
                          Fad.FxAlMaSetting ->
                          (Fad.FxTechnicalAnalysisData -> Fad.FxMovingAverageData) ->
                          [Fad.FxTechnicalAnalysisData] ->
                          Fad.FxMovingAverageData
setFxMovingAverageData short middle long tmin tmax ftms g pdl =
  let n  = Fad.prevSetting ftms
      fmadp = g $ head pdl
      fmad = Fad.FxMovingAverageData { Fad.short      = short
                                     , Fad.middle     = middle
                                     , Fad.long       = long
                                     , Fad.slopeS     = lsm n (map (Fad.short  . g) $ take n pdl)
                                     , Fad.slopeM     = lsm n (map (Fad.middle . g) $ take n pdl)
                                     , Fad.slopeL     = lsm n (map (Fad.long   . g) $ take n pdl)
                                     , Fad.slopeSn    = lsmn $ map (Fad.slopeS . g) (take n pdl) ++ [Fad.slopeS fmad]
                                     , Fad.slopeMn    = lsmn $ map (Fad.slopeM . g) (take n pdl) ++ [Fad.slopeM fmad]
                                     , Fad.slopeLn    = lsmn $ map (Fad.slopeL . g) (take n pdl) ++ [Fad.slopeL fmad]
                                     , Fad.crossSL    = setCross short  long   (Fad.short  fmadp) (Fad.long   fmadp)
                                     , Fad.crossSM    = setCross short  middle (Fad.short  fmadp) (Fad.middle fmadp)
                                     , Fad.crossML    = setCross middle long   (Fad.middle fmadp) (Fad.long   fmadp)
                                     , Fad.thresholdS = setThreshold short  tmin tmax ftms
                                     , Fad.thresholdL = setThreshold middle tmin tmax ftms
                                     , Fad.thresholdM = setThreshold long   tmin tmax ftms
                                     }
  in fmad

makeFxMovingAverageData :: (Int -> [Fcd.FxChartData] -> Double) ->
                           Double ->
                           Double ->
                           [Fcd.FxChartData] ->
                           Fad.FxAlMaSetting ->
                           (Fad.FxTechnicalAnalysisData -> Fad.FxMovingAverageData) ->
                           [Fad.FxTechnicalAnalysisData] ->
                           Fad.FxMovingAverageData
makeFxMovingAverageData f tmin tmax lr ftms g pdl =
  let short  = f (Fad.shortSetting ftms)  lr
      middle = f (Fad.middleSetting ftms) lr
      long   = f (Fad.longSetting ftms)   lr
  in setFxMovingAverageData short middle long tmin tmax ftms g pdl

makeFxTechnicalAnalysisData :: Fad.FxAlgorithmSetting ->
                               [Fcd.FxChartData] ->
                               Fcd.FxChartData ->
                               [Fad.FxTechnicalAnalysisData] ->
                               Fad.FxTechnicalAnalysisData
makeFxTechnicalAnalysisData ftas lr chart pdl =
  let (macd, macdSignal) = getMACD (Fad.middle $ Fad.ema x) (Fad.long $ Fad.ema x) (Fad.middleSetting $ Fad.macdSetting ftas) pdl
      (k, d, sd) = getST (Fad.longSetting $ Fad.stSetting ftas) (Fad.middleSetting $ Fad.stSetting ftas) lr pdl
      x = Fad.FxTechnicalAnalysisData { Fad.chart  = chart
                                      , Fad.sma    = makeFxMovingAverageData getSma 0 0 lr (Fad.smaSetting ftas) Fad.sma pdl
                                      , Fad.ema    = makeFxMovingAverageData getEma 0 0 lr (Fad.emaSetting ftas) Fad.ema pdl
                                      , Fad.wma    = makeFxMovingAverageData getWma 0 0 lr (Fad.wmaSetting ftas) Fad.wma pdl
                                      , Fad.macd   = setFxMovingAverageData macd 0 macdSignal 0 0 (Fad.macdSetting ftas) Fad.macd pdl
                                      , Fad.rci    = makeFxMovingAverageData getRci (-100) 100 lr (Fad.rciSetting ftas) Fad.rci pdl
                                      , Fad.st     = setFxMovingAverageData k d sd  0 100 (Fad.stSetting ftas) Fad.st pdl
                                      , Fad.rsi    = makeFxMovingAverageData getRsi 0 100 lr (Fad.rsiSetting ftas) Fad.rsi pdl
                                      , Fad.bb     = getBB (Fad.middleSetting $ Fad.smaSetting ftas) (Fad.middle $ Fad.sma x) lr
                                      }
  in if null pdl
     then Fad.initFxTechnicalAnalysisData
     else x

{-
lf [old .. new]
lr [new .. old]
-}

makeFxTechnicalAnalysisDataList :: Fad.FxAlgorithmSetting ->
                                   [Fcd.FxChartData] ->
                                   [Fcd.FxChartData] ->
                                   [Fad.FxTechnicalAnalysisData] ->
                                   [Fad.FxTechnicalAnalysisData]
makeFxTechnicalAnalysisDataList _  _             [] x = x
makeFxTechnicalAnalysisDataList fs lr (lf:lfs) x =
  let d = makeFxTechnicalAnalysisData fs (lf:lr) lf x
  in  makeFxTechnicalAnalysisDataList fs (lf:lr) lfs (d:x)

