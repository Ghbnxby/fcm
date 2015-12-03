module Modules.Random (
  MatrixSize,
  VectorLength,
  getRandVector,
  getRandMatrix,
  getRandVectorWithoutReps
) where


  import System.Random
  import Data.List


  type MatrixSize = (Int, Int)
  type VectorLength = Int
  type Range = (Int, Int)


  getRandVectorWithoutReps :: VectorLength -> Range -> IO [Int]
  getRandVectorWithoutReps size range = do
    stdGen <- getStdGen
    return $ take size . nub $ (randomRs range stdGen :: [Int])


  getRandVector :: VectorLength -> IO [Double]
  getRandVector size = do
    stdGen <- newStdGen
    let vector = take size (randomRs (0, 1) stdGen :: [Double])
        vectorSum = sum vector
    return $ map (/ vectorSum) vector


  getRandMatrix :: MatrixSize -> IO [[Double]]
  getRandMatrix size = recursive (fst size) []
    where
      recursive count mtrx =
        if count == 0 then return mtrx
        else do
          line <- getRandVector $ snd size
          recursive (count - 1) (line : mtrx)
