module Modules.Fcm (
  Metric (..),
  FcmOptions (..),
  execFcm,
  getHemmingDistance,
  getEuclidianDistance
) where


  import qualified Data.Vector as V
  import Data.Matrix
  import Modules.Random


  data Metric = Hemming | Euclid deriving(Show, Read)
  data FcmOptions = FcmOptions {  clasterCount :: Int,
                                  metric :: Metric,
                                  accuracy :: Double,
                                  isRandCenters :: Bool }

  type ObjectMatrix = Matrix Double
  type SuppliesMatrix = [[Double]]
  type Centers = [[Double]]



  execFcm :: ObjectMatrix -> FcmOptions -> IO (Matrix Double)
  execFcm objMatrix fcmOpts = do
    let rowCount = nrows objMatrix
        numOfClusters = clasterCount fcmOpts

    initSupMatrix <- if isRandCenters fcmOpts then
                        getSupMatrixOfRandCenters objMatrix fcmOpts
                     else getRandMatrix (rowCount, numOfClusters)

    let next oldSupMtrx =  let nextCenters = getNextCenters oldSupMtrx objMatrix fcmOpts
                               supMtrx = getNextSupMatrix nextCenters objMatrix fcmOpts
                               tmp = map (\i -> map (\j -> abs(supMtrx !! i !! j - oldSupMtrx !! i !! j)) [0..numOfClusters - 1]) [0..rowCount - 1]
                               eps = maximum $ map maximum tmp

                           in  if eps <= accuracy fcmOpts then fromLists supMtrx
                               else next supMtrx

    return $ next initSupMatrix



  getNextCenters :: SuppliesMatrix -> ObjectMatrix -> FcmOptions -> Centers
  getNextCenters supMtrx objMtrx fcmOpts = centers
    where
      rowLength = ncols objMtrx
      rowCount = nrows objMtrx
      xRow i = V.toList $ getRow i objMtrx
      mulVectAndScalar v s = map (* s) v
      scalar l = 1 / sum (map (\i -> ((supMtrx !! i) !! l) ** 2) [0..rowCount - 1])

      total l i res =
        if i == 0 then res
        else
          let vector = map (* (supMtrx !! (i - 1) !! l) ** 2) (xRow i)
              prevRes = if null res then replicate rowLength 0 else res
              result = zipWith (+) prevRes vector
          in total l (i - 1) result

      center l = mulVectAndScalar (total l rowCount []) (scalar l)
      centers = map center [0..(clasterCount fcmOpts - 1)]



  getNextSupMatrix :: Centers -> ObjectMatrix -> FcmOptions -> SuppliesMatrix
  getNextSupMatrix centers objMatrix fcmOpts = result
    where
      distance = case metric fcmOpts of
                    Euclid -> getEuclidianDistance
                    Hemming -> getHemmingDistance

      element xi vk = if xi == vk then 1
                         else
                          let numerator = distance xi vk
                              denominator = distance xi
                              sumElem vj = (numerator / denominator vj) ** 2
                          in 1 / sum (map sumElem centers)

      supMatrixRow xi = map (element xi) centers
      result = map supMatrixRow (toLists objMatrix)



  getHemmingDistance :: [Double] -> [Double] -> Double
  getHemmingDistance frstVector scndVector =
    sum $ map (\ pair -> abs (snd pair - scndVector !! fst pair)) (zip [0..] frstVector)



  getEuclidianDistance :: [Double] -> [Double] -> Double
  getEuclidianDistance frstVector scndVector =
    sqrt $ sum $ map (\ pair -> (snd pair - scndVector !! fst pair) ** 2) (zip [0..] frstVector)



  getSupMatrixOfRandCenters :: ObjectMatrix -> FcmOptions -> IO SuppliesMatrix
  getSupMatrixOfRandCenters objMatrix fcmOpts = do
    let numOfClasters = clasterCount fcmOpts
    tmp <- if numOfClasters <= nrows objMatrix then
              getRandVectorWithoutReps numOfClasters (1, nrows objMatrix)
           else
              getRandVectorWithoutReps (nrows objMatrix - 1) (1, nrows objMatrix)

    let rowLength = ncols objMatrix
        tmpLength = length tmp
        missingCenters = replicate (numOfClasters - tmpLength) (replicate rowLength 0)
        centers = map (\ x -> V.toList $ getRow x objMatrix) tmp ++ missingCenters
    return $ getNextSupMatrix centers objMatrix fcmOpts
