module HUpdate.Options (Opts (..), parseOpts) where

import Options.Applicative
import Data.Monoid ((<>))

data Opts = Opts 
    { output     :: FilePath -- Where HUpdate should put the database
    , dbRoot     :: FilePath -- Where to start indexing
    , cfgFile    :: FilePath -- Config file to read
    }

parseOpts :: IO Opts
parseOpts = execParser infoOpts
    where infoOpts = info (helper <*> opts) (fullDesc <> progDesc "Update database for hlocate")

opts :: Parser Opts
opts = Opts 
    <$> strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Write the database to FILE instead of using the default database."
       <> value "/etc/hlocate.db" )
    <*> strOption
        ( long "database-root"
       <> short 'U'
       <> metavar "PATH"
       <> help "Store only the results of scanning the file system subtree rooted at PATH to the generated database. The whole filesystem is scanned by default"
       <> value "/" )
    <*> strOption
        ( long "config-file"
       <> short 'C'
       <> metavar "FILE"
       <> help "Specify configuration file to read from (/etc/hlocate.conf by default)"
       <> value "/etc/hlocate.conf" )
