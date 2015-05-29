{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Monad
import Network
import System.IO
import Text.Printf 
import System.Process
import System.Directory
import Data.Bool
import qualified Data.Foldable as F
import Data.List
import Data.List.Split
import System.FilePath
import Control.Exception

import Page404

fileDir = "./files/"

shrinkPath :: String -> Maybe String
shrinkPath = fmap (("./" ++) . intercalate "/" . reverse) . F.foldlM body_ [] . splitOn "/" . tail
  where
      body_ []     ".." = Nothing
      body_ (x:xs) ".." = Just xs
      body_ xs     y    = Just (y:xs)

getPage :: Handle -> FilePath -> IO ()
getPage h "./" = getPage h "./index.hs"
getPage h f    = doesFileExist (fileDir ++ f) >>= getPage'
  where
    getPage' :: Bool -> IO ()
    getPage' False = do
        hPutStr h "HTTP/1.1 404 Not Found"
        generate404 h
    getPage' True  = 
      case takeExtensions f of
        ".hs" -> do
             hPutStr h "HTTP/1.1 200 OK"
             createProcess (proc "runghc" ["-i ..", f])
               { std_in  = UseHandle h
               , std_out = UseHandle h
               , std_err = UseHandle stderr
               , cwd = Just fileDir}
             return ()
        _     -> hPutStr h "HTTP/1.1 403 Forbidden\r\n\r\n"
handleRequest h = do
    req <- try $ hGetLine h
    case req of
      Left (_ :: IOException) -> do
          print "Socket closed prematurely."
      Right req'              -> do
          let [type_, file, version] = words req'
          case shrinkPath file of
            Just file' -> 
              case type_ of
                "GET" -> getPage h file'
                _     -> hPutStr stderr "Not supported"
    hClose h

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 80
    forever $ do
      (h, _, _) <- accept sock
      forkIO $ handleRequest h
