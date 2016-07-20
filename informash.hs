import System.Environment
import System.Random
import System.IO
import System.IO.Error
import System.Exit
import Data.List
import Data.Maybe
import System.Console.GetOpt
import Text.Read    -- for readMaybe
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as Bytes
import Data.Binary
import Data.Digest.Pure.MD5
import Data.Int
import qualified Data.Map as Map


import Token
import TextStats
import Histogram
import Util
import WordRamp
import WordTable

version = "1.00beta"
copyright = "Chris Reuter (C) 2016"

genTokens :: WordTable -> StdGen -> Bool -> [Token]
genTokens tbl gen para =
  let rnds  = randoms gen :: [Double]
      start = pickStartingWord (head rnds) para tbl
      words _ [] = []
      words prev (idx:rest) =
        let next = pickNextWord prev idx tbl
        in prev : words next rest
  in words start $ tail rnds


genSequence :: WordTable -> Int -> StdGen -> String
genSequence tbl count gen =
  untokenize $ take count $ tail $ genTokens tbl gen False

-- restricted to 2000 tokens to prevent runaway
genChunk :: ([Token] -> [Token]) -> WordTable -> StdGen -> Bool -> [Token]
genChunk chopfn tbl gen para =
  take 2000 $ chopfn $ drop 1 $ genTokens tbl gen para

genSentence :: WordTable -> StdGen -> String
genSentence tbl gen =
  untokenize $ filter (not.isParagraphBreak) chunks where
    chunks = genChunk (takeUpTo isEnder) tbl gen False

genSentences :: WordTable -> Int -> StdGen -> [String]
genSentences tbl count gen =
  map (genSentence tbl) $ take count $ splits gen

genParagraph :: WordTable -> StdGen -> String
genParagraph tbl gen =
  untokenize $ genChunk (takeWhile (not.isParagraphBreak)) tbl gen True

genParagraphs :: WordTable -> Int -> StdGen -> [String]
genParagraphs tbl count gen =
  map (genParagraph tbl) $ take count $ splits gen




data CmdOpt = CmdOpt {optTable :: String,
                      optMkTable :: Bool,
                      optOutput :: String,
                      optWords  :: Int,
                      optSen :: Int,
                      optPara :: Int,
                      optDump :: String,
                      optSeed :: Int,
                      optHelp :: Bool,
                      optVersion :: Bool,
                      optError :: [String]} deriving (Show)


outputUnset :: CmdOpt -> Bool
outputUnset opts =
  let count = foldr (+) 0 $ map (min 1) [optSen opts,optPara opts,optWords opts]
  in count == 0


intOpt :: String -> (Int -> CmdOpt -> CmdOpt) -> String -> CmdOpt -> CmdOpt
intOpt optName setter arg opt =
  let checkSign n       = if n > 0 then Just n else Nothing
      checkUnset opt n  = if outputUnset opt then Just n else Nothing
      intArg            = (readMaybe arg :: Maybe Int) >>=
                          checkUnset opt >>=
                          checkSign
  in case intArg of
      Just n    -> setter n opt
      Nothing   -> opt { optError = optError opt ++
                                    ["Invalid '--" ++ optName ++ "'."]}
      

options :: [ OptDescr (CmdOpt -> CmdOpt) ]
options =
  let op   short long fn desc = Option short [long] (NoArg fn) desc
      op_s short long con fn meta desc =
        Option short [long] (con fn meta) desc
      op_i short long con fn meta desc =
        Option short [long] (con (intOpt long fn) meta) desc

  in [
    op_s "t" "table" ReqArg (\arg opt -> opt { optTable = arg }) "FILE"
      "Name of the word-table file.",

    op "m" "make-table" (\opt -> opt { optMkTable = True })
      "Create the initial word table.",

    op_i "w" "words" ReqArg (\arg opt -> opt { optWords = arg })
      "COUNT" "Emit COUNT tokens.",
    
    op_i "s" "sentences" ReqArg (\arg opt -> opt { optSen = arg })
      "COUNT" "Emit COUNT sentences.",

    op_i "p" "paragraphs" ReqArg (\arg opt -> opt { optPara = arg })
      "COUNT" "Emit COUNT paragraphs.",

    op_s "d" "debug-table" ReqArg (\arg opt -> opt { optDump = arg }) "FILE"
      "Save table to FILE as text.",

    op_i "r" "seed" ReqArg (\arg opt -> opt { optSeed = arg }) "SEED"
      "Use SEED for the random number generator.",

    op "h" "help" (\opt -> opt { optHelp = True })
      "Display this text.",

    op "v" "version" (\opt -> opt { optVersion = True })
      "Display the current version and exit."
    ]


                                                     
getCmdlineOptions :: [String] -> (CmdOpt, [String])
getCmdlineOptions argv =
  let (actions, argv2, errors) = getOpt Permute options argv
      defaults  = CmdOpt "informash.wtbl" False "" 0 0 0 "" 0 False False errors
      opts      = foldl (\opts trnsfrm -> trnsfrm opts) defaults actions
  in (opts, argv2)


generateOutput :: CmdOpt -> WordTable -> StdGen -> [String]
generateOutput opt tbl gen
  | optPara opt > 0     =  genParagraphs tbl (optPara opt) gen
  | optWords opt > 0    = [genSequence   tbl (optWords opt) gen]
  | optSen opt > 0      =  genSentences  tbl (optSen opt) gen
  | otherwise           =  genSentences  tbl 1 gen



handleFor :: String -> IOMode -> IO Handle
handleFor "" ReadMode   = return stdin
handleFor "" WriteMode  = return stdout
handleFor "" _          = error "Unsupported file type"     -- Not Allowed
handleFor path mode     = openFile path mode
  

load :: [String] -> IO TextStats
load files = do
  strings <- mapM readFile files

  let addTxt ts txt = addTokens (tokenize txt) ts
      stats = foldl addTxt emptyTextStats strings

  return stats


dumpIfRequested :: CmdOpt -> WordTable -> IO ()
dumpIfRequested opts tbl =
  when (optDump opts /= "") $ writeFile (optDump opts) (printableForm tbl)


writeTable :: FilePath -> WordTable -> IO ()
writeTable path tbl =
  let bytes     = encode tbl
      checksum  = encode $ md5 bytes
      sumlen    = fromInteger (toInteger $ Bytes.length checksum) :: Word8
      items = Bytes.cons sumlen $ Bytes.concat [checksum, bytes]
  in Bytes.writeFile path items


decodeTable :: Bytes.ByteString -> Maybe WordTable
decodeTable bytes = do
  let len = Bytes.length bytes
  sumlen <-     if len > 0
                then return ((fromIntegral $ Bytes.head bytes) :: Int64)
                else Nothing

  sum <-        if sumlen > 0 && len > sumlen
                then return
                     ((decode $ Bytes.take sumlen $ Bytes.tail bytes)::MD5Digest)
                else Nothing

  let tblbytes = Bytes.drop (sumlen + 1) bytes
  return $ decode tblbytes


readTable :: FilePath -> IO WordTable
readTable path = do
  bytes <- Bytes.readFile path
  tbl   <- return $ decodeTable bytes
  maybe
    (ioError $ userError ("Table file '" ++ path ++ "' is corrupt."))
    return
    tbl


makeTable :: CmdOpt -> [String] -> IO ()
makeTable opts justArgs = do
  stats <- load justArgs
  
  let tbl = newWordTable stats
  dumpIfRequested opts tbl

  writeTable (optTable opts) tbl

  return ()



  
getRnd :: CmdOpt -> IO StdGen
getRnd opts
  | optSeed opts > 0    = return $ mkStdGen $ optSeed opts
  | otherwise           = getStdGen


generateAndPrint :: CmdOpt -> IO ()
generateAndPrint opts = do
  tbl <- readTable (optTable opts)

  dumpIfRequested opts tbl

  gen0 <- getRnd opts
  output <- handleFor (optOutput opts) WriteMode
  let result = intercalate "\n\n" $ generateOutput opts tbl gen0
  hPutStr output ("\n" ++ result ++ "\n\n")

  return ()
  
  
main' :: IO ()
main' = do
  args <- getArgs
  let (opts, justArgs) = getCmdlineOptions args

  when (optError opts /= []) $
    ioError (userError $ intercalate "\n" (optError opts))

  let crMsg = "Copyright " ++ copyright ++ "; GNU GPL v2; NO WARRANTY!"
  
  when (optHelp opts) $ do
    prg <- getProgName
    hPutStrLn stdout (usageInfo prg options)
    hPutStrLn stdout crMsg
    exitSuccess

  when (optVersion opts) $ do
    prg <- getProgName
    let msg = prg ++ " " ++ version ++ "; " ++ crMsg
    hPutStrLn stdout msg
    exitSuccess

  if optMkTable opts
    then makeTable opts justArgs
    else generateAndPrint opts


errorHandler :: IOError -> IO ()
errorHandler e = do
  let fn = fromMaybe "" (ioeGetFileName e >>= (\x -> Just (x ++ " ")))
  putStrLn ("Error: " ++ fn ++ ioeGetErrorString e)
  return ()


main :: IO ()
main = main' `catch` errorHandler
