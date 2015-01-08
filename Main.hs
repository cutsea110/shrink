{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Char
import Control.Exception
import Control.Monad
import Data.Typeable
import Graphics.Exif
import Graphics.GD
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO (hPutStr, hPutStrLn, stderr)

data Opt = Opt
           { optVerbose     :: Bool
           , optShowVersion :: Bool
           , optOutDir      :: FilePath
           , optWidth       :: Int
           , optHeight      :: Int
           } deriving Show
                      
defaultOpt :: Opt
defaultOpt = Opt False False "./out" 800 480

optSpec :: [OptDescr (Opt -> Opt)]
optSpec =
  [ Option ['v'] ["verbose"]
    (NoArg (\opt -> opt {optVerbose = True}))
    "chatty output on stderr"
  , Option ['V','?'] ["version"]
    (NoArg (\opt -> opt {optShowVersion = True}))
    "show version number"
  , Option ['o'] ["outdir"]
    (ReqArg (\o opt -> opt {optOutDir = o}) "FILEPATH")
    "output directory"
  , Option ['w'] ["width"]
    (ReqArg (\w opt -> opt {optWidth = read w}) "INT")
    "photoframe lcd width"
  , Option ['h'] ["height"]
    (ReqArg (\h opt -> opt {optHeight = read h}) "INT")
    "photoframe lcd height"
  ]

data CmdError = NoSuchCommand String
              | CmdArg [String]
              | IllegalArg String
              | FileNotExist deriving (Show, Typeable)
instance Exception CmdError
  
parseArgs :: [OptDescr (Opt -> Opt)] -> [String] -> (Opt, [FilePath])
parseArgs spec argv = case getOpt Permute spec argv of
  (o,n,[]  ) -> (foldl (flip id) defaultOpt o, n)
  (_,_,errs) -> throw (CmdArg errs)


main :: IO ()
main = flip catches handlers $ do
  argv <- getArgs
  let (opt, cmdArg) = parseArgs optSpec argv
      inDir = if length cmdArg > 0 then head cmdArg else throw FileNotExist
  proc opt inDir
    where
      handlers = [Handler handler0, Handler handler1]
      handler0 :: ErrorCall -> IO ()
      handler0 e = print e -- for debug
      handler1 :: CmdError -> IO ()
      handler1 (NoSuchCommand cmd) = hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
      handler1 (CmdArg errs) = mapM_ (hPutStr stderr) errs >> printUsage
      handler1 (IllegalArg err) = hPutStrLn stderr err >> printUsage
      handler1 FileNotExist = hPutStrLn stderr "input image directory not found" >> printUsage
      printUsage = hPutStrLn stderr $ "\n" ++ usageInfo usage optSpec
      usage = "shrink version 0.1.0\nUsage:"
       
proc :: Opt -> FilePath -> IO ()
proc opt fromDir = do
  files <- getDirectoryContents fromDir
  forM_ files $ \f -> do
    if isJPG f
      then do
      img <- loadJpegFile $ fromDir </> f
      exif <- fromFile $ fromDir </> f
      mo <- getTag exif "Orientation"
      img' <- case mo of
        Just "Top-left" -> return img
        Just "Right-top" -> rotateImage 3 img
        Just "Left-bottom" -> rotateImage 1 img
        Just "Bottom-right" -> rotateImage 2 img
        _ -> return img
      (w, h) <- imageSize img'
      let size' = newSize (w, h)
      img'' <- uncurry resizeImage size' img'
      createDirectoryIfMissing True toDir
      saveJpegFile (-1) (toDir </> f) img''
      else if isAVI f
           then copyFile (fromDir</>f) (toDir</>f)
           else return ()
  where
    toDir = optOutDir opt
    ext = map toUpper . takeExtension
    isJPG f = ".JPG" == ext f
    isAVI f = ".AVI" == ext f
    (wMax, hMax) = (optWidth opt, optHeight opt)
    newSize (w, h) | w <= wMax || h <= hMax = (w, h)
                   | 6*w >= 10*h = (wMax, wMax*h`div`w)
                   | otherwise = (hMax*w`div`h, hMax)
