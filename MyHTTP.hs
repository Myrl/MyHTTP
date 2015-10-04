module MyHTTP (contentLength, contentType, headerEnd, hContentType, hHeaderEnd, hContentLength) where

import System.IO
import Text.Printf

contentType :: String -> IO ()
contentType = putStr . printf "\r\nContent-type: %s"

contentLength :: Int -> IO ()
contentLength = putStr . printf "\r\ncontent-length: %d"

headerEnd :: IO ()
headerEnd = putStr "\r\n\r\n"

hContentType :: Handle -> String -> IO ()
hContentType h = hPutStr h . printf "\r\nContent-type: %s"

hContentLength :: Handle -> Int -> IO ()
hContentLength h = hPutStr h .printf "\r\ncontent-length: %d"

hHeaderEnd :: Handle -> IO ()
hHeaderEnd h = hPutStr h "\r\n\r\n"
