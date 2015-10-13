{-# LANGUAGE OverloadedStrings #-}
module MyHTTP (Request(Get), Connection(KeepAlive, Close), Response(connection, contentType), sRespond, okResponse, notImplementedResponse, notFoundResponse, sResponseCode, sContentLength, sContentType) where

import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B (pack)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Maybe

data Request =
    Get (Maybe B.ByteString) (Maybe B.ByteString) (Int, Int) -- Path, Version
    deriving (Show)

data Response = Response
    { responseCode  :: Int
    , connection    :: Connection
    , contentType   :: Maybe B.ByteString
    }

data Connection = KeepAlive Int | Close

godResponse :: Response
godResponse = Response { responseCode  = 0
                       , connection    = MyHTTP.KeepAlive 15
                       , contentType   = Nothing 
                       }

notImplementedResponse :: Response
notImplementedResponse = godResponse { responseCode = 501 }

okResponse :: Response
okResponse = godResponse{ responseCode = 200 }

notFoundResponse :: Response
notFoundResponse = godResponse{ responseCode = 404 }

-- hDoMaybe :: (a -> IO ()) -> Maybe a -> IO ()
-- hDoMaybe = maybe (return ())

sRespond :: Socket -> Response -> Maybe B.ByteString -> IO ()
sRespond s r x = do
    sendMany s foo
    where
        foo = 
          [ sResponseCode  $ responseCode r
          , sConnection    $ connection  r
          ] ++ (catMaybes
          [ sContentType   <$> contentType r
          , sContentLength <$> x
          , Just "\r\n\r\n"
          , x
          ])
    --sendAll s . sResponseCode $ responseCode r
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
sConnection (MyHTTP.KeepAlive x) = B.append "\r\nConnection: keep-alive\r\nTimeout: " (B.pack $ show x)
