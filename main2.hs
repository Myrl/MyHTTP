{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Char
import Network
import System.IO
import System.Directory
import Data.Bool
import Data.List
import System.FilePath
import Data.Function
import qualified Control.Exception as E
import Text.Parsec
import qualified Data.Foldable as F

data Request =
    Get (Maybe String) (Int, Int) -- Path, Version
    deriving (Show)

blank = satisfy go <?> "blank"
    where
        go '\n' = False
        go x    = isSpace x

blanks = skipMany blank
keyword s = try (string s) *> blanks

headerGetParser = Get <$ keyword "GET" <*> path <*> version
    where
        path    = shrinkPath <$ char '/' <*> sepBy (many $ noneOf " /") (char '/') <* blanks
        version = ((,) `on` read) <$ keyword "HTTP/" <*> many1 digit <* char '.' <*> many1 digit

headerParser :: Parsec String () Request
headerParser = try headerGetParser

shrinkPath :: [String] -> Maybe String
shrinkPath = fmap (('/':) =<<) . F.foldrM body_ []
  where
      body_ ".." []     = Nothing
      body_ ".." (x:xs) = Just xs
      body_ "."  xs     = Just xs
      body_ ""   xs     = Just xs
      body_ x    xs     = Just (x:xs)

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 80
    forever $ do
        (h, _, _) <- accept sock
        req <- E.try $ hGetLine h
        case req of
          Left  (e :: E.IOException) ->
              print $ "IO Error encountered: " ++ show e
          Right req                  ->
              asdas
    

