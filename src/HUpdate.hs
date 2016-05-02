{-# LANGUAGE LambdaCase #-}

module Main where

import HUpdate.Options (parseOpts, Opts (..))
import HUpdate.WalkDir (walkDirPrune)
import HUpdate.Config (readConfig)

import Control.Monad.Trans.Reader (runReaderT, asks, ReaderT)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Pipes
import Pipes.Binary (encode)
import System.IO
import System.MountPoints (getMounts, Mntent (..))

import qualified Data.Map.Strict  as M
import qualified Pipes.ByteString as PB

main :: IO ()
main = putStrLn "Indexing..." >> parseOpts >>= runReaderT updateDB

-- Index directory and encode to output file
updateDB :: ReaderT Opts IO ()
updateDB = do out <- asks output 
              f <- asks cfgFile >>= liftIO . getPrunes 
              asks dbRoot >>= liftIO . withFile out WriteMode . encodeFiles f
    where encodeFiles f r h = runEffect $ for (walkDirPrune f r) encode >-> PB.toHandle h

-- Take config file location and return single pruning function
getPrunes :: FilePath -> IO (FilePath -> Bool)
getPrunes fi =  readConfig fi >>= 
    \case Right cfg -> do let vars = fmap words . flip M.lookup cfg <$> ["pruneNames", "prunePaths", "pruneFS"]
                          let fs   = [return . mkPrune isInfixOf, return . mkPrune (==), isFs]
                          fs' <- sequenceA . catMaybes $ zipWith fmap fs vars
                          return (\x -> any ($ x) fs')
          Left  err -> hPrint stderr err >>
                       hPutStrLn stderr "\nContinuing without pruning..." >>
                       return (const False)

-- Take list of filesystem names and returns a function that takes a mountpoint and returns whether it is that filesystem
isFs :: [String] -> IO (FilePath -> Bool)
isFs fis = flip elem . map mnt_dir . filter ((`elem` fis) . mnt_fsname) <$> getMounts

-- Take a function return a function that applies it with an argument to a list and returns whether it ever returned True
mkPrune :: (a -> a -> Bool) -> ([a] -> a -> Bool)
mkPrune f = flip (any . flip f)
