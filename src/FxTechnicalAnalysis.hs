module FxTechnicalAnalysis
  ( makeFxTechnicalAnalysisDataList
  , makeValidLeafDataMapInc
  , makeValidLeafDataMapDec
  , calcFxalgorithmListCount
  , updateAlgorithmListCount
  , checkAlgoSetting
  , getLearningTestTime
  , getPrepareTimeAll
  , getHoldTime
  ) where

import           Control.Monad
import           Control.Monad.Random    as R
import           Data.List
import qualified Data.Map                as M
import           Debug.Trace
import qualified FxSettingData           as Fsd
import qualified FxChartData             as Fcd
import qualified FxTechnicalAnalysisData as Fad
import qualified GlobalSettingData       as Gsd
import qualified Tree                    as Tr

getLearningTestTime :: Fsd.FxSettingData -> Int
getLearningTestTime fsd =
  let fs = Fsd.learningSetting $ Fsd.fxSetting fsd
      t = Fsd.getLearningTestTimes fsd * if Fsd.numTraderadeDate fs == 0
                                         then 60
                                         else (Fsd.totalTradeDate fs `div` Fsd.numTraderadeDate fs)
  in if Gsd.maxTradeTime Gsd.gsd < t
     then Gsd.maxTradeTime Gsd.gsd
     else t


getHoldTime :: Fsd.FxSettingData -> Int
getHoldTime fsd = 
  getPrepareTime . Fsd.fxTaOpen        $ Fsd.fxSetting fsd

getPrepareTimeAll :: Fsd.FxSettingData -> Int
getPrepareTimeAll fsd =
  maximum [ getPrepareTime . Fsd.fxTaOpen        $ Fsd.fxSetting fsd
          , getPrepareTime . Fsd.fxTaCloseProfit $ Fsd.fxSetting fsd
          , getPrepareTime . Fsd.fxTaCloseLoss   $ Fsd.fxSetting fsd
          ] 

getPrepareTime :: Fad.FxTechnicalAnalysisSetting -> Int
getPrepareTime x =
  maximum $ M.map (\a -> maximum [ Fad.longSetting (Fad.rciSetting a)
                                 , Fad.longSetting (Fad.smaSetting a)
                                 , Fad.longSetting (Fad.emaSetting a)
                                 , Fad.longSetting (Fad.rsiSetting a)
                                 , Fad.longSetting (Fad.stSetting a) 
                                 ] * Fad.getSimChartMax x) $ Fad.algoSetting x

checkAlgoSetting :: R.MonadRandom m => Fad.FxTechnicalAnalysisSetting -> m (Fad.FxTechnicalAnalysisSetting)
checkAlgoSetting fts = do
  let as  = Fad.algoSetting fts
      tlc = Fad.techListCount fts
  (as'', pr) <- foldl (\acc k -> do (as', p) <- acc
                                    let x = as M.! k
                                        (a, b) = Tr.checkLeafDataMap $ Fad.algorithmListCount x
                                        x' = x { Fad.algorithmListCount = Tr.addLeafDataMap b p }
                                        t = Tr.adjustTree (Fad.algorithmListCount x') (Fad.algorithmTree x)
                                    t' <- if t == Tr.Empty
                                          then do taAndR <- getRandomR(max 1 (Fad.algorithmAndRate x' - Gsd.taMargin Gsd.gsd), 1 + Fad.algorithmAndRate x' + Gsd.taMargin Gsd.gsd)
                                                  taOrR  <- getRandomR(max 1 (Fad.algorithmOrRate  x' - Gsd.taMargin Gsd.gsd), 1 + Fad.algorithmOrRate  x' + Gsd.taMargin Gsd.gsd)
                                                  Tr.makeTree taAndR taOrR (Fad.algorithmListCount x') Tr.Empty
                                          else return t
                                    let x'' = x { Fad.algorithmTree = t' }
                                    return (M.insert k x'' as', a)) (pure (as, Tr.emptyLeafDataMap))
                . sort $ M.keys as
  let (as''', tlc') =  if not . M.null $ Tr.getLeafDataMap pr
                       then let nk = fst (M.findMax as) + 1
                                tlcl = Tr.getLeafDataMap tlc
                                ave = (foldr (\(acc, _) a -> acc + a) 0 tlcl) / (fromIntegral $ length tlcl)
                            in (M.insert nk (Fad.initFxAlgorithmSetting pr) as'',
                                Tr.LeafDataMap $ M.insert (Fad.initTechAnaLeafData nk) (ave, 0) tlcl)
                      else (as'', tlc)
  return $ fts { Fad.techListCount = tlc'
               , Fad.algoSetting   = as'''
               }

updateAlgorithmListCount :: (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) ->
                            Fad.FxChartTaData ->
                            (Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData),
                             M.Map Int (Tr.LeafDataMap Fad.FxTechnicalAnalysisData)) ->
                            Fad.FxTechnicalAnalysisSetting ->
                            Fad.FxTechnicalAnalysisSetting
updateAlgorithmListCount f ctd (ldlt, ldla) fts =
  let tlc = Tr.addLeafDataMap (Fad.techListCount fts) ldlt
      as  = M.foldrWithKey (\k x acc -> let y = acc M.! k
                                            y' = y { Fad.algorithmListCount =
                                                     Tr.addLeafDataMap x (Fad.algorithmListCount y) }
                                        in M.insert k y' acc)
            (updateThreshold f ctd $ Fad.algoSetting fts) ldla
  in fts { Fad.techListCount = tlc
         , Fad.algoSetting   = as
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

calcFxalgorithmListCount :: Double ->
                           ([Tr.LeafData (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData)],
                            M.Map Int [Tr.LeafData Fad.FxTechnicalAnalysisData]) ->
                           (Tr.LeafDataMap (M.Map Int Fad.FxAlgorithmSetting, M.Map Int Fad.FxTechnicalAnalysisData),
                            M.Map Int (Tr.LeafDataMap Fad.FxTechnicalAnalysisData))
calcFxalgorithmListCount p (ptat, pat) =
  (Tr.calcValidLeafDataList p ptat, M.map (\x -> Tr.calcValidLeafDataList p x) pat)

getThreshold :: Double ->
                Double -> 
                Int ->
                Fad.FxChartTaData ->
                (Fad.FxTechnicalAnalysisData -> Fad.FxMovingAverageData) ->
                (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) ->
                Double ->
                Double
getThreshold a b k x f1 f2 p =
  if M.member k $ f2 x
  then ((b - abs ((Fad.short  . f1 $ f2 x M.! k) - a)) +
        (b - abs ((Fad.middle . f1 $ f2 x M.! k) - a)) +
        (b - abs ((Fad.long   . f1 $ f2 x M.! k) - a)) + p) / 4
  else p

updateThreshold :: (Fad.FxChartTaData -> M.Map Int Fad.FxTechnicalAnalysisData) ->
                   Fad.FxChartTaData ->
                   M.Map Int Fad.FxAlgorithmSetting ->
                   M.Map Int Fad.FxAlgorithmSetting
updateThreshold f ctd =
  M.mapWithKey (\k x -> x { Fad.stSetting  = (Fad.stSetting x)
                            { Fad.thresholdMaxSetting = getThreshold 50 50 k ctd Fad.st f . Fad.thresholdMaxSetting $ Fad.stSetting x
                            }
                          , Fad.rciSetting = (Fad.rciSetting x)
                            { Fad.thresholdMaxSetting = getThreshold 0 100 k ctd Fad.rci f . Fad.thresholdMaxSetting $ Fad.rciSetting x
                            }
                          , Fad.rsiSetting = (Fad.rsiSetting x)
                            { Fad.thresholdMaxSetting = getThreshold 50 50 k ctd Fad.rsi f . Fad.thresholdMaxSetting $ Fad.rsiSetting x
                            }
                          })

rci :: Int -> [Double] -> Double
rci n x  =
  let r  = [1..n] :: [Int]
      r' = reverse [1..n] :: [Int]
      d = sum . map (\(a, b) -> (a - b) ^ (2 :: Int)) . zipWith (\a (_, b') -> (a, b')) r' . sort $ zip x r
  in (1 - (6.0 * fromIntegral d) / (fromIntegral n * (fromIntegral n ^ (2 :: Int) - 1))) * 100

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
  in if upa + downa == 0
     then 50
     else (100 * upa) / (upa + downa)

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
  in if length s1 < n || length s2 < m || maximum s1 == minimum s1
     then (50, 50, 50)
     else (k, d, sd)

setCross :: Double ->
            Double ->
            Double ->
            Double ->
            Fad.FxTradePosition
setCross s l sp lp
  | sp < lp && l < s = Fad.Buy
  | lp < sp && s < l = Fad.Sell
  | otherwise = Fad.None

setThreshold :: Double ->
                Double ->
                Double ->
                Fad.FxAlMaSetting ->
                Fad.FxTradePosition
setThreshold x tmin tmax ftms
  | x < tmin + Fad.thresholdSetting ftms = Fad.Buy
  | tmax - Fad.thresholdSetting ftms < x = Fad.Sell
  | otherwise                            = Fad.None

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
  let fmadp = g $ head pdl
      fmad = Fad.FxMovingAverageData { Fad.short      = short
                                     , Fad.middle     = middle
                                     , Fad.long       = long
                                     , Fad.crossSL    = setCross short  long   (Fad.short fmadp)  (Fad.long fmadp)  
                                     , Fad.crossSM    = setCross short  middle (Fad.short fmadp)  (Fad.middle fmadp)
                                     , Fad.crossML    = setCross middle long   (Fad.middle fmadp) (Fad.long fmadp)  
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

