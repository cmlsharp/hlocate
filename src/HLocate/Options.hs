module HLocate.Options (Opts (..), parseOpts) where

import Options.Applicative

data Opts = Opts 
    { location :: String
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
    <*> some (argument str (metavar "QUERIES..."))
