module Main where

  import Test.Hspec
  import Modules.Fcm
  import Modules.Random
  import Data.Matrix
  import Data.Vector as V
  import Data.List as L


  main :: IO ()
  main = hspec $ do

    describe "Distance" $ do
      describe "Hemming" $ do
        it "Hemming ([2, 2, 2, 2], [4, 4, 4, 4]) == 8" $
          getHemmingDistance [2, 2, 2, 2] [4, 4, 4, 4] `shouldBe` 8.0

        it "Hemming ([1, 2], [3, 4]) == 4" $
            getHemmingDistance [1, 2] [3, 4] `shouldBe` 4.0

      describe "Euclid" $ do
        it "Euclid ([2, 2, 2, 2], [4, 4, 4, 4]) == 4" $
          getEuclidianDistance [2, 2, 2, 2] [4, 4, 4, 4] `shouldBe` 4.0

        it "Euclid ([1, 2], [0, 0]) == sqrt 5" $
          getEuclidianDistance [1, 2] [0, 0] `shouldBe` sqrt 5


    describe "Supplies matrix" $ do
      it "Verify size" $ do
        let objMatrix = fromLists [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]
            fcmOpts = FcmOptions {  clasterCount = 3,
                                    metric = Euclid,
                                    accuracy = 0.0001,
                                    isRandCenters = False }

        supMatrix <- execFcm objMatrix fcmOpts
        nrows supMatrix `shouldBe` nrows objMatrix
        ncols supMatrix `shouldBe` clasterCount fcmOpts

      it "The sum of the line items must be 1" $ do
        let objMatrix = fromLists [[1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], [9.0, 10.0, 11.0, 12.0]]
            fcmOpts = FcmOptions {  clasterCount = 3,
                                    metric = Euclid,
                                    accuracy = 0.0001,
                                    isRandCenters = False }

        supMatrix <- execFcm objMatrix fcmOpts
        let result = L.sum (L.map (\i -> V.sum $ getRow i supMatrix ) [1..nrows supMatrix]) / fromIntegral (nrows supMatrix)
        result `shouldBe` 1.0


    describe "Random vector" $
      it "Vector values is not repeated" $ do
        vector <- getRandVectorWithoutReps 5 (1, 15)
        let result = nub vector == vector
        result `shouldBe` True
