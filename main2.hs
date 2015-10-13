{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile)
import System.Random
import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Attoparsec.ByteString.Char8
--import qualified Data.IntMap as IntMap
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F

import MyHTTP

homeDir :: B.ByteString
homeDir = "/home/myrl/Website/files"

blanks :: Parser ()
blanks = pure () <* takeWhile go <?> "blanks"
    where
        go '\r' = False
        go '\n' = False
        go x    = isSpace x

keyword :: B.ByteString -> Parser ()
keyword s = string s *> blanks <?> "keyword"

headerGetParser :: Parser Request
headerGetParser = Get <$ keyword "GET" <*> path <*> args <*> version <* flushGet <?> "headerGetParser"
    where
        path     = shrinkPath <$ slash <*> sepBy fileName slash <* blanks
        fileName = takeWhile1 $ notInClass " /?"
        slash    = char '/'
        args     = option Nothing $ Just <$ char '?' <*> takeTill (== ' ') <* blanks
        version  = (,) <$ keyword "HTTP/" <*> decimal <* satisfy (inClass ".") <*> decimal <?> "version"
        
        flushGet = (manyTill anyChar $ string "\r\n\r\n") <?> "flushGet"

headerParser :: Parser Request
headerParser = try headerGetParser

shrinkPath :: [B.ByteString] -> Maybe B.ByteString
shrinkPath = fmap (B.append homeDir . B.concat . fmap (B.cons '/')) . F.foldrM body_ []
  where
      body_ ".." []     = Nothing
      body_ ".." (_:xs) = Just xs
      body_ "."  xs     = Just xs
      body_ ""   xs     = Just xs
      body_ x    xs     = Just (x:xs)

takeExt :: Parser B.ByteString
takeExt = last <$> takeWhile1 (/= '.') `sepBy` char '.' 

--takeExt :: Parser B.ByteString
--takeExt = takeTill (== '.') *> char '.' *> takeByteString
    
handleRequest :: Socket -> B.ByteString -> Int -> IO ()
handleRequest s r x =
--    parseWith (recv s 1024 >>= (\x -> print (B.append "Recv: " x) >> return x)) (match headerParser) r >>= \case
--      (Fail _ es e) -> return () --error $ e ++ (concatMap (" <- " ++) $ es)
--      (Done r' (str, Get (Just path) _ _))  -> do
--          print $ B.append "Consumed: " str
--          print $ "id: " ++ show x
--          print $ B.append "r: " r'
--          let (Right ext) = parseOnly takeExt path
--          fileHandler ext path
--          handleRequest s r' x
    go $ parse headerParser r
    where
        go (Fail _ es e) = error $ e ++ concatMap (" <- " ++ ) es
        go (Partial   f) = recv s 1024 >>= go' f
        go (Done r' (Get (Just path) _ _)) = do
            let (Right ext) = parseOnly takeExt path
            fileHandler ext path
            handleRequest s r' x
        go' f str
            | B.length str == 0 = print ("Connection closed." :: B.ByteString)
            | otherwise        = go (f str)
        fileHandler "png"  = fileHandler' "image/png"
        fileHandler "jpeg" = fileHandler' "image/jpeg"
        fileHandler "jpg"  = fileHandler' "image/jpeg"
        fileHandler "js"   = fileHandler' "application/javascript"
        fileHandler "html" = fileHandler' "text/html"
        fileHandler "css"  = fileHandler' "text/css"
        fileHandler "ttf"  = fileHandler' "application-font-sfnt"
        fileHandler "woff" = fileHandler' "application/font-woff"
        fileHandler _       = niHandler
        fileHandler' cType path = do
            print path
            file <- B.readFile . B.unpack $ path
            sRespond s (okResponse { --connection    = Close
                                    contentType   = Just cType
                                   }) $ Just file
        niHandler _ = sRespond s (notFoundResponse)  Nothing

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReusePort 1
    bind sock (SockAddrInet 8080 iNADDR_ANY)
    listen sock 5
    forever $ do
        E.bracketOnError (accept sock)
                         (\(s, _) -> close s)
                         (\(s, _) -> (randomIO :: IO Int) >>= handleRequest s "")
