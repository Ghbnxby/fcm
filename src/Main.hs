module Main where


  import Modules.Fcm as FCM
  import Modules.CsvParser as Csv
  import Data.Matrix
  import Options.Applicative
  import Data.Strings
  import System.IO
  import System.Directory
  import Control.Exception


  data ConsoleOptions = ConsoleOptions {  inputFile :: String,
                                          outputFile :: String,
                                          numOfClaster :: Int,
                                          accuracy :: Double,
                                          metric :: Metric,
                                          isRandCenters :: Bool,
                                          delimiter :: Char,
                                          isIgnoreHeader :: Bool,
                                          isIgnoreFirstColumn :: Bool,
                                          isIgnoreLastColumn :: Bool }


  main :: IO ()
  main = do
    let options = info (helper <*> parseConsoleOptions) fullDesc
    consoleOptions <- execParser options

    isExistFile <- doesFileExist $ inputFile consoleOptions

    if not isExistFile then
      putStrLn $ "File " ++ inputFile consoleOptions ++ " does not exist"
    else do
      handler <- openFile (inputFile consoleOptions) ReadMode
      hSetEncoding handler utf8_bom

      src <- hGetContents handler
      csv <- parseCsv src (createCSVParserOptions consoleOptions)

      case csv of
        Left err -> putStrLn err
        Right objMatrix -> do
          supMatrix <- execFcm objMatrix (createFcmOptions consoleOptions)
          if strNull $ outputFile consoleOptions then
            putStrLn $ prettyMatrix supMatrix
          else
            writeFile (outputFile consoleOptions) (prettyMatrix supMatrix)  `catch` errHandler
              where
                errHandler :: IOError -> IO()
                errHandler = print


      hClose handler


  createCSVParserOptions :: ConsoleOptions -> ParserOptions
  createCSVParserOptions opts = ParserOptions { Csv.delimiter = Main.delimiter opts,
                                                Csv.isIgnoreHeader = Main.isIgnoreHeader opts,
                                                Csv.isIgnoreFirstColumn = Main.isIgnoreFirstColumn opts,
                                                Csv.isIgnoreLastColumn = Main.isIgnoreLastColumn opts  }



  createFcmOptions :: ConsoleOptions -> FcmOptions
  createFcmOptions opts = FcmOptions {  clasterCount = numOfClaster opts,
                                        FCM.metric = Main.metric opts,
                                        FCM.accuracy = Main.accuracy opts,
                                        FCM.isRandCenters = Main.isRandCenters opts}



  parseConsoleOptions :: Parser ConsoleOptions
  parseConsoleOptions = ConsoleOptions
    <$> argument str
        (  metavar "FILE"
        <> help "Source file" )
    <*> option str
        (  long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Result output File"
        <> value "" )
    <*> option auto
        (  long "numOfClusters"
        <> short 'c'
        <> metavar "COUNT"
        <> help "Claster count. Default 3"
        <> value 3 )
    <*> option auto
        (  long "accuracy"
        <> short 'a'
        <> metavar "VALUE"
        <> help "Accuracy. Default 0.0001"
        <> value 0.0001 )
    <*> option auto
        (  long "metric"
        <> short 'm'
        <> metavar "NAME"
        <> help "Hemming or Euclid. Default Hemming"
        <> value Hemming )
    <*> option auto
        (  long "isRandCenters"
        <> short 'r'
        <> metavar "BOOL"
        <> help "How to start the algorithm. True - if at random centers, False - with random supplies matrix. Default False"
        <> value False)
    <*> option auto
            (  long "delimiter"
            <> short 'd'
            <> metavar "CHAR"
            <> help "Csv parser. Delimiter. Default ','"
            <> value ',')
    <*> option auto
        (  long "isIgnoreHeader"
        <> short 'h'
        <> metavar "BOOL"
        <> help "Csv parser. Ignored header. True - ignored, False - not ignored. Default False"
        <> value False)
    <*> option auto
        (  long "isIgnoreFirstColumn"
        <> short 'f'
        <> metavar "BOOL"
        <> help "Csv parser. Ignored first Column. True - ignored, False - not ignored. Default False"
        <> value False)
    <*> option auto
        (  long "isIgnoreLastColumn"
        <> short 'l'
        <> metavar "BOOL"
        <> help "Csv parser. Ignored last column. True - ignored, False - not ignored. Default False"
        <> value False)
