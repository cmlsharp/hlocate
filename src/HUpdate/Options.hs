module HUpdate.Options (Opts (..), parseOpts) where

import Options.Applicative

data Opts = Opts 
    { output     :: FilePath
    , dbRoot     :: FilePath
    }

parseOpts :: IO Opts
parseOpts = execParser infoOpts
    where infoOpts = info (helper <*> opts) (fullDesc <> progDesc "Update database for hlocate")

opts :: Parser Opts
opts = Opts 
    <$> strOption
        ( long "database"
       <> short 'd'
       <> metavar "FILE"
       <> help "Write the database to FILE instead of using the default database."
       <> value "/etc/hlocate.db" )
    <*> strOption
        ( long "database-root"
       <> short 'U'
       <> metavar "PATH"
       <> help "Store only the results of scanning the file system subtree rooted at PATH to the generated database. The whole filesystem is scanned by default"
       <> value "/" )
