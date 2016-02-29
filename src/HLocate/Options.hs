module HLocate.Options (Opts (..), parseOpts) where

import Options.Applicative

data Opts = Opts 
    { location :: String
    , testFunc :: [Bool] -> Bool
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
       <> help "Print only entries that match all QUERIES instead of requiring only one of them to match")
    <*> some (argument str (metavar "QUERIES..."))
