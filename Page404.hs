module Page404 (generate404) where

import System.IO
import qualified Data.ByteString as B
import MyHTTP
import Control.Concurrent

generate404 h = do
    hContentType h "image/jpg"
    image <- B.readFile "./files/404.png"
    hContentLength h $ B.length image
    hHeaderEnd h
    B.hPutStr h image
