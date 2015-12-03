# FCM


####Command line
```
Usage: FCM FILE [-o|--output FILE] [-c|--numOfClusters COUNT]
           [-a|--accuracy VALUE] [-m|--metric NAME] [-r|--isRandCenters BOOL]
           [-d|--delimiter CHAR] [-h|--isIgnoreHeader BOOL]
           [-f|--isIgnoreFirstColumn BOOL] [-l|--isIgnoreLastColumn BOOL]

Available options:
  -h,--help                     Show this help text
  FILE                          Source file
  -o,--output FILE              Result output File
  -c,--numOfClusters COUNT      Claster count. Default 3
  -a,--accuracy VALUE           Accuracy. Default 0.0001
  -m,--metric NAME              Hemming or Euclid. Default Hemming
  -r,--isRandCenters BOOL       How to start the algorithm. True - if at random centers, False - with random supplies matrix. Default False
  -d,--delimiter CHAR           Csv parser. Delimiter. Default ','
  -h,--isIgnoreHeader BOOL      Csv parser. Ignored header. True - ignored, False - not ignored. Default False
  -f,--isIgnoreFirstColumn BOOL Csv parser. Ignored first Column. True - ignored, False - not ignored. Default False
  -l,--isIgnoreLastColumn BOOL  Csv parser. Ignored last column. True - ignored, False - not ignored. Default False
```
###To run project
In project directory ```(cd /d 'path to project')``` type in console:
```
cabal sandbox init
cabal install --only-dependencies
cabal build
./dist/build/FCM/FCM 'csv with source data'
```
###To run test
Type in console:
```
cabal install --enable-tests
cabal test
```

