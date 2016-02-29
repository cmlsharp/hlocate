module HUpdate.Config (Cfg, readConfig) where

import Control.Monad (liftM2)
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

import qualified Data.Map.Strict as M

type Cfg = M.Map String String

ident :: Parser String
ident = liftM2 (:) (letter <|> char '_') (many (letter <|> digit <|> char '_')) <?> "identifier"

comment :: Parser ()
comment = char '#' >> skipMany (noneOf "\r\n") <?> "comment"

eol :: Parser ()
eol = oneOf "\n\r" >> return () <?> "end of line"

item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, reverse . dropWhile isSpace . reverse $ value)

line :: Parser (Maybe (String, String))
line = skipMany space >> (try (comment >> return Nothing) <|> (item >>= return . Just))

file :: Parser [(String, String)]
file = many line >>= return . catMaybes

readConfig :: SourceName -> IO (Either ParseError Cfg)
readConfig name = parseFromFile file name >>= 
    return . fmap (foldr (uncurry M.insert) M.empty . reverse)
