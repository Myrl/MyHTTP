{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile)
import Data.Bool
import System.Random
import Control.Monad
import System.Directory
import Control.Concurrent
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Text.Encoding
--import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as B
import Network.Mime

import MyHTTP
import Parser
    
handleRequest :: Socket -> B.ByteString -> Int -> IO ()
handleRequest s r x =
    recv s 1024 >>= go . parse headerParser . B.append r
    where
        go (Fail _ es e) = close s >>= (error $ e ++ concatMap (" <- " ++ ) es)
        go (Partial   f) = recv s 1024 >>= \case
           ""  -> print ("Connection closed." :: B.ByteString)
           str -> go (f str)
        go (Done _ (Head _ _ _)) = niHandler ()
        go (Done _ (Get Nothing _ _)) = nfHandler ()
        go (Done r' (Get (Just path) _ _)) = do
            fileHandler . defaultMimeLookup . decodeUtf8 <*> B.unpack $ path
            --((. (defaultMimeLookup . decodeUtf8)) . flip fileHandler =<< B.unpack) path
            --fileHandler (defaultMimeLookup . decodeUtf8 $ path) $ B.unpack path
            handleRequest s r' x
        fileHandler :: B.ByteString -> String -> IO ()
        fileHandler cType path =
            doesFileExist path >>= (bool
                (nfHandler ()) $ 
                Just <$> B.readFile path >>=
                   sRespond 200 s (header { contentType = Just cType
                                           }))
        nfHandler _ = sRespond 404 s (header) Nothing
        niHandler _ = sRespond 501 s (header) Nothing

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReusePort 1
    bind sock (SockAddrInet 80 iNADDR_ANY)
    listen sock 5
    forever $ do
        (s, _) <- accept sock
        forkIO $ randomIO >>= handleRequest s ""
