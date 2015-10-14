{-# LANGUAGE OverloadedStrings #-}
module MyHTTP (Request(Get, Head), Connection(KeepAlive, Close), Header(connection, contentType), header, sRespond, sResponseCode, sContentLength, sContentType) where

import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B (pack)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Maybe

data Request = Get (Maybe B.ByteString) (Maybe B.ByteString) (Int, Int) -- Path, Version
             | Head (Maybe B.ByteString) (Maybe B.ByteString) (Int, Int)
    deriving (Show)

data Header = Header
    { connection    :: Connection
    , contentType   :: Maybe B.ByteString
    }

data Connection = KeepAlive Int | Close

header :: Header
header = Header { connection    = MyHTTP.KeepAlive 15
                   , contentType   = Nothing 
                   }

-- hDoMaybe :: (a -> IO ()) -> Maybe a -> IO ()
-- hDoMaybe = maybe (return ())

sRespond :: Int -> Socket -> Header -> Maybe B.ByteString -> IO ()
sRespond i s r x = do
    sendMany s foo
    where
        foo = 
          [ sResponseCode i
          , sConnection    $ connection  r
          ] ++ (catMaybes
          [ sContentType   <$> contentType r
          , sContentLength <$> x
          , Just "\r\n\r\n"
          , x
          ])
    --sendAll s . sHeaderCode $ responseCode r
    --sendAll s . sConnection $ connection r
    --hDoMaybe (sendAll s . sContentType)   $ contentType   r
    --hDoMaybe (sendAll s . sContentLength) $ contentLength r
    --sendAll s  "\r\n\r\n"
      
sResponseCode :: Int -> B.ByteString
sResponseCode 200 = "HTTP/1.1 200 OK"
sResponseCode 404 = "HTTP/1.1 404 Not Found"
sResponseCode _   = "HTTP/1.1 501 Not Implemented"

sContentType :: B.ByteString -> B.ByteString
sContentType = B.append "\r\nContent-type: "

sContentLength :: B.ByteString -> B.ByteString
sContentLength = B.append "\r\ncontent-length: " . B.pack . show . B.length

sConnection :: Connection -> B.ByteString
sConnection Close                = "\r\nConnection: Close"
sConnection (MyHTTP.KeepAlive x) = B.concat ["\r\nConnection: keep-alive\r\nKeep-Alive: timeout=", (B.pack $ show x), ",max=100"]
