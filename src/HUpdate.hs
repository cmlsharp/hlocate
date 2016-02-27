module Main where

import HUpdate.Options
import HUpdate.WalkDir (walkDirPrune)

import Data.List (isInfixOf)
import Pipes
import Pipes.Binary (encode)
import System.IO (withFile, IOMode (..))


import qualified Pipes.ByteString as PB

main = parseOpts >>= updateDB <$> output <*> dbRoot

updateDB :: FilePath -> FilePath -> IO ()
updateDB out root = withFile out WriteMode encodeFiles
    where encodeFiles h = runEffect $ for (walkDirPrune (isInfixOf ".snapshots") root) encode >-> PB.toHandle h

