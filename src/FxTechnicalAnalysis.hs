module FxTechnicalAnalysis
  ( makeFxTechnicalAnalysisDataList
  ) where

--import Debug.Trace
import Data.List
import qualified FxChartData              as Fcd
import qualified FxTechnicalAnalysisData  as Fad

rci :: Int -> [Double] -> Double
rci n x  =
  let r  = [1..n]
      r' = reverse [1..n]
      d = sum . map (\(a, b) -> (a - b) ^ (2 :: Int)) . zipWith (\a (_, b') -> (a, b')) r' . sort $ zip x r 
  in (1 - (6.0 * fromIntegral d) / ((fromIntegral n) * ((fromIntegral n) ^ (2 :: Int) - 1))) * 100

lsm :: Int -> [Double] -> Double
lsm n y =
  let x = reverse $ take n [1..]
  in ((fromIntegral n) * (sum $ zipWith (*) x y) - sum x * sum y) / ((fromIntegral n) * (sum $ map (^(2 :: Int)) x) - (sum x) ^ (2 :: Int))

getRci :: Int -> [Fcd.FxChartData] -> Double
getRci n x =
  let s = take n $ map Fcd.close x
  in if length s < n
     then 0
     else rci n s

rsiUpDown :: Double ->  [Double] -> (Double, Double)
rsiUpDown _ []     = (0, 0)
rsiUpDown p (x:[]) =
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
  let (up, down) = rsiUpDown (head $ reverse x) (tail $ reverse x)
      upa = up / (fromIntegral n)
      downa = down / (fromIntegral n)
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
     else ((sum s) + h) / ((fromIntegral n) + 1)

getWma :: Int -> [Fcd.FxChartData] -> Double
getWma n x =
  let s = take n $ map Fcd.close x
      r = reverse [1..n]
  in if length s < n
     then 0
     else (sum . map (\(a, b) -> a * fromIntegral b) $ zip s r) / (fromIntegral $ sum r)
          
getMACD :: Double -> Double -> Int -> [Fad.FxTechnicalAnalysisData] -> (Double, Double)
getMACD es el n x =
  let macd = if es ==0 || el == 0
             then 0
             else es - el
      signal = let s = take (n - 1) x
               in if length s < (n - 1) || macd == 0
                  then 0
                  else ((sum $ map (Fad.short . Fad.macd) s) + macd) / (fromIntegral n)
  in (macd, signal)

getBB :: Int -> Int -> Double -> [Fcd.FxChartData] -> (Double, Double)
getBB n a ma x =
  let s = take n $ map Fcd.close x
      sd = sqrt $ ((fromIntegral n) * (foldl (\acc b -> (b ^ (2 :: Int) + acc)) 0 s) - (sum s) ^ (2 :: Int)) / (fromIntegral $ n * (n - 1))
  in if length s < n || ma == 0
     then (0, 0)
     else (ma + sd * (fromIntegral a), ma - sd * (fromIntegral a))


getST :: Int -> Int -> [Fcd.FxChartData] -> [Fad.FxTechnicalAnalysisData] -> (Double, Double, Double)
getST n m x p =
  let s1 = take n $ map Fcd.close x -- setting long
      s2 = take m p -- setting short
      k  = ((head s1 - minimum s1) * 100 / (maximum s1 - minimum s1)) -- short
      d  = (sum $ map (Fad.short  . Fad.st) s2) / (fromIntegral m)    -- middle
      sd = (sum $ map (Fad.middle . Fad.st) s2) / (fromIntegral m)    -- long
  in if length s1 < n || length s2 < m
     then (50, 50, 50)
     else (k, d, sd)

setCross :: Double ->
            Double ->
            Double ->
            Double ->
            Fad.FxTradePosition
setCross s l ps pl =
  if ps < pl && l < s
  then Fad.Buy
  else if pl < ps && s < l
       then Fad.Sell
       else Fad.None

lsmn :: [Double] -> Fad.FxTradePosition
lsmn xs =
  if and $ map (0 <) xs
  then Fad.Buy
  else if and $ map (< 0) xs
       then Fad.Sell
       else Fad.None

setThreshold :: Double ->
                Double ->
                Double ->
                Fad.FxAlMaSetting ->
                Fad.FxTradePosition
setThreshold x tmin tmax ftms =
  if x < tmin + Fad.thresholdSetting ftms
  then Fad.Buy
  else if tmax - Fad.thresholdSetting ftms < x
       then Fad.Sell
       else Fad.None
            
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
                                     , Fad.slopeSn    = lsmn $ (map (Fad.slopeS . g) $ take n pdl) ++ [Fad.slopeS fmad]
                                     , Fad.slopeMn    = lsmn $ (map (Fad.slopeM . g) $ take n pdl) ++ [Fad.slopeM fmad]
                                     , Fad.slopeLn    = lsmn $ (map (Fad.slopeL . g) $ take n pdl) ++ [Fad.slopeL fmad]
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
                               [Fcd.FxChartData] ->
                               [Fad.FxTechnicalAnalysisData] ->
                               Fad.FxTechnicalAnalysisData
makeFxTechnicalAnalysisData ftas lr lf pdl =
  let chart = head lf
      (macd, macdSignal) = getMACD (Fad.short $ Fad.ema x) (Fad.long $ Fad.ema x) (Fad.shortSetting $ Fad.macdSetting ftas) pdl
      (bbp3a, bbm3a) = getBB (Fad.shortSetting $ Fad.smaSetting ftas) 2 (Fad.short $ Fad.sma x) lr
      (k, d, sd) = getST (Fad.longSetting $ Fad.stSetting ftas) (Fad.shortSetting $ Fad.stSetting ftas) lr pdl
      x = Fad.FxTechnicalAnalysisData { Fad.chart = chart
                                      , Fad.sma   = makeFxMovingAverageData getSma 0 0 lr (Fad.smaSetting ftas) Fad.sma pdl
                                      , Fad.ema   = makeFxMovingAverageData getEma 0 0 lr (Fad.emaSetting ftas) Fad.ema pdl
                                      , Fad.wma   = makeFxMovingAverageData getWma 0 0 lr (Fad.wmaSetting ftas) Fad.wma pdl
                                      , Fad.macd  = setFxMovingAverageData macd 0 macdSignal 0 0 (Fad.macdSetting ftas) Fad.macd pdl
                                      , Fad.rci   = makeFxMovingAverageData getRci (-100) 100 lr (Fad.rciSetting ftas) Fad.rci pdl
                                      , Fad.st    = setFxMovingAverageData k d sd  0 100 (Fad.stSetting ftas) Fad.st pdl
                                      , Fad.rsi   = makeFxMovingAverageData getRsi 0 100 lr (Fad.rsiSetting ftas) Fad.rsi pdl
                                      , Fad.bbp3a = bbp3a
                                      , Fad.bbm3a = bbm3a
                                      }
  in x

makeFxTechnicalAnalysisDataList :: Fad.FxAlgorithmSetting ->
                                   [Fcd.FxChartData] ->
                                   [Fcd.FxChartData] ->
                                   [Fad.FxTechnicalAnalysisData] ->
                                   [Fad.FxTechnicalAnalysisData]
makeFxTechnicalAnalysisDataList _  _             [] _ = []
makeFxTechnicalAnalysisDataList fs lr lfAll@(lf:[]) x =
  let d = makeFxTechnicalAnalysisData fs (lf:lr) lfAll x
  in  d:x
makeFxTechnicalAnalysisDataList fs lr lfAll@(lf:lfs) [] =
  let d = makeFxTechnicalAnalysisData fs (lf:lr) lfAll [Fad.initFxTechnicalAnalysisData]
  in  makeFxTechnicalAnalysisDataList fs (lf:lr) lfs (d:[])
makeFxTechnicalAnalysisDataList fs lr lfAll@(lf:lfs) x =
  let d = makeFxTechnicalAnalysisData fs (lf:lr) lfAll x
  in  makeFxTechnicalAnalysisDataList fs (lf:lr) lfs (d:x)

