module HLocate.Options (Opts (..), parseOpts) where

import Options.Applicative
import Data.List (isInfixOf)
import System.FilePath (takeBaseName)
import Text.Regex.PCRE ((=~))

data Opts = Opts 
    { location  :: String
    , andOr     :: [Bool] -> Bool
    , baseName  :: FilePath -> String
    , matchFunc :: String -> String -> Bool
    , endChar   :: Char
    , queries   :: [String]
    }

parseOpts :: IO Opts
parseOpts = execParser infoOpts
    where infoOpts = info (helper <*> opts) (fullDesc <> progDesc "Find files by name")

opts :: Parser Opts
opts = Opts 
    <$> strOption
        ( long "database"
       <> short 'd'
       <> metavar "DBPATH"
       <> help "Specify an alternative DBPATH"
       <> value "/etc/hlocate.db" )
    <*> flag or and
        ( long "all"
       <> short 'A'
       <> help "Print only entries that match all QUERIES instead of requiring only one of them to match" )
    <*> flag id takeBaseName
        ( long "basename"
       <> short 'b'
       <> help "Match only the base name against the specified patterns." )
    <*> flag isInfixOf (flip (=~))
        ( long "regex"
       <> short 'r'
       <> help "Use regular expressions"          
        )
    <*> flag '\n' '\0'
        ( long "null"
       <> short '0'
       <> help "Separate entries with NULL on output" )
    <*> some (argument str (metavar "QUERIES..."))
