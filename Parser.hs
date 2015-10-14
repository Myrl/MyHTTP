{-# LANGUAGE OverloadedStrings #-}
module Parser(headerParser, takeExt) where

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import Control.Applicative

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

headerHeadParser :: Parser Request
headerHeadParser = Head <$ keyword "HEAD" <*> path <*> args <*> version <* flushGet <?> "headerHeadParser"

path :: Parser (Maybe B.ByteString)
path = shrinkPath <$ slash <*> sepBy fileName slash <* blanks

fileName :: Parser B.ByteString
fileName = takeWhile1 $ notInClass " /?"

slash :: Parser Char
slash = char '/'

args :: Parser (Maybe B.ByteString)
args = option Nothing $ 
              Just <$ char '?' <*> takeTill (== ' ') <* blanks

version :: Parser (Int, Int)
version  = (,) <$ keyword "HTTP/" <*> decimal <* satisfy (inClass ".") <*> decimal <?> "version"

flushGet :: Parser ()
flushGet = pure () <* (manyTill anyChar $ string "\r\n\r\n") <?> "flushGet"

headerParser :: Parser Request
headerParser = choice [ headerGetParser
                      , headerHeadParser
                      ]

shrinkPath :: [B.ByteString] -> Maybe B.ByteString
shrinkPath = fmap (B.append homeDir . B.concat . fmap (B.cons '/')) . F.foldrM body_ []
  where
      body_ ".." []     = Nothing
      body_ ".." (_:xs) = Just xs
      body_ "."  xs     = Just xs
      body_ ""   xs     = Just xs
      body_ x    xs     = Just (x:xs)

takeExt :: Parser B.ByteString
takeExt = takeWhile1 (/= '.') *> char '.' *> takeExt <|> takeByteString
