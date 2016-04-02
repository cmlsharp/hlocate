module HUpdate.Config (Cfg, readConfig) where

import Control.Monad (liftM2)
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

import qualified Data.Map.Strict as M

type Cfg = M.Map String String

-- An identfier begins with a letter or underscore and contains letters, underscores, and numbers
ident :: Parser String
ident = liftM2 (:) (letter <|> char '_') (many (letter <|> digit <|> char '_')) <?> "identifier"

-- Comments begin with # and ignore everything else
comment :: Parser ()
comment = char '#' >> skipMany (noneOf "\r\n") <?> "comment"

-- EOL is either a newline or a carraige return
eol :: Parser ()
eol = oneOf "\n\r" >> return () <?> "end of line"

-- Items parses "ident = value" and returns (ident, value)
item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, reverse . dropWhile isSpace . reverse $ value)

--  Lines contain items or comments
line :: Parser (Maybe (String, String))
line = skipMany space >> (try (comment >> return Nothing) <|> (item >>= return . Just))

-- Files are a bunch of lines, ignoreing commments
file :: Parser [(String, String)]
file = many line >>= return . catMaybes

-- Returns a Parse error or a Map of idents to values
readConfig :: SourceName -> IO (Either ParseError Cfg)
readConfig name = parseFromFile file name >>= 
    return . fmap (foldr (uncurry M.insert) M.empty . reverse)
