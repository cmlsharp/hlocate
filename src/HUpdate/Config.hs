module HUpdate.Config (Cfg, readConfig) where

import Data.Maybe
import Data.Char
import Text.ParserCombinators.Parsec

import qualified Data.Map.Strict as M

type Cfg = M.Map String String

ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_')
           return (c:cs)
        <?> "identifier"

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")
          <?> "comment"

eol :: Parser ()
eol = do oneOf "\n\r"
         return ()
      <?> "end of line"

item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, reverse . dropWhile isSpace . reverse $ value)

line :: Parser (Maybe (String, String))
line = do skipMany space
          try (comment >> return Nothing) <|> (item >>= return . Just)

file :: Parser [(String, String)]
file = do lines <- many line
          return (catMaybes lines)

readConfig :: SourceName -> IO (Either ParseError Cfg)
readConfig name = 
    parseFromFile file name >>=
        return . fmap (foldr (uncurry M.insert) M.empty . reverse)
