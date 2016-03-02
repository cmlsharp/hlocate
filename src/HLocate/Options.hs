module HLocate.Options (Opts (..), parseOpts) where

import Options.Applicative
import Data.List (isInfixOf)
import System.FilePath (takeBaseName)

data Opts = Opts 
    { location :: String
    , andOr    :: [Bool] -> Bool
    , testFunc :: FilePath -> FilePath -> Bool
    , endChar  :: Char
    , queries  :: [String]
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
    <*> flag isInfixOf (\x y -> isInfixOf x $ takeBaseName y)
        ( long "basename"
       <> short 'b'
       <> help "Match only the base name against the specified patterns." )
    <*> flag '\n' '\0'
        ( long "null"
       <> short '0'
       <> help "Separate entries with NULL on output" )
    <*> some (argument str (metavar "QUERIES..."))
