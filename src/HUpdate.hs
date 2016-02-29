{-# LANGUAGE LambdaCase #-}

module Main where

import HUpdate.Options
import HUpdate.WalkDir (walkDirPrune)
import HUpdate.Config (Cfg, readConfig)

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Pipes
import Pipes.Binary (encode)
import System.IO
import System.MountPoints
import Text.ParserCombinators.Parsec.Error (messageString, errorMessages)

import qualified Data.Map.Strict  as M
import qualified Pipes.ByteString as PB

main = putStrLn "Indexing..." >> parseOpts >>= updateDB <$> output <*> dbRoot <*> cfgFile

updateDB :: FilePath -> FilePath -> FilePath -> IO ()
updateDB out root cfg = getPrunes cfg >>= (\f -> withFile out WriteMode $ encodeFiles f)
    where encodeFiles f h = runEffect $ for (walkDirPrune f root) encode >-> PB.toHandle h

getPrunes :: FilePath -> IO (FilePath -> Bool)
getPrunes fi =  readConfig fi >>= 
    \case Right cfg -> do let vars = fmap words . flip M.lookup cfg <$> ["pruneNames", "prunePaths", "pruneFS"]
                          let fs   = [return . mkPrune isInfixOf, return . mkPrune (==), isFs]
                          fs <- sequenceA . catMaybes $ zipWith fmap fs vars
                          return (\x -> any ($ x) fs)
          Left  err -> hPutStrLn stderr "WARNING: Configuration file parsing failed with the following error(s). Continuing without pruning\n" >>
                       hPutStrLn stderr (unlines . map messageString . errorMessages $ err) >> 
                       return (const False)

isFs :: [String] -> IO (FilePath -> Bool)
isFs fs = flip elem . map mnt_dir . filter ((`elem` fs) . mnt_fsname) <$> getMounts

mkPrune :: (a -> a -> Bool) -> [a] -> a -> Bool
mkPrune f xs y = any (`f` y) xs
