module Modules.CsvParser (
  parseCsv,
  ParserOptions(..),
) where



import Data.Char (ord)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Internal (unpackBytes)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Matrix as M



data ParserOptions = ParserOptions {  delimiter :: Char,
                                      isIgnoreHeader :: Bool,
                                      isIgnoreFirstColumn :: Bool,
                                      isIgnoreLastColumn :: Bool  }



parseCsv :: String -> ParserOptions -> IO(Either String (Matrix Double))
parseCsv csvData parserOpts = do
  let decodeOptions = DecodeOptions { decDelimiter = fromIntegral (ord $ delimiter parserOpts) }
      csvBytes = BL.pack . unpackBytes . pack $ csvData

  let result = if isIgnoreHeader parserOpts
                  then decodeWith decodeOptions HasHeader csvBytes
                  else decodeWith decodeOptions NoHeader csvBytes

  case result of
      Left exception -> return (Left exception)
      Right table -> let withoutFirstColumn = if isIgnoreFirstColumn parserOpts then
                                                V.map V.tail table else table

                         withoutLastColumn = if isIgnoreLastColumn parserOpts then
                                                V.map V.init withoutFirstColumn else withoutFirstColumn
                     in return (Right $ toDoubleMatrix withoutLastColumn)



toDoubleMatrix :: V.Vector(V.Vector String) -> Matrix Double
toDoubleMatrix table =  fromLists doubleList
                        where doublesV = V.map (V.map $ \ x -> read x :: Double) table
                              doubleList = V.toList $ V.map V.toList doublesV
