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

main = putStrLn "Indexing..." >> parseOpts >>= runReaderT updateDB

updateDB :: ReaderT Opts IO ()
updateDB = do out <- asks output 
              fs <- asks cfgFile >>= liftIO . getPrunes 
              asks dbRoot >>= liftIO . withFile out WriteMode . encodeFiles fs
    where encodeFiles f r h = runEffect $ for (walkDirPrune f r) encode >-> PB.toHandle h

getPrunes :: FilePath -> IO (FilePath -> Bool)
getPrunes fi =  readConfig fi >>= 
    \case Right cfg -> do let vars = fmap words . flip M.lookup cfg <$> ["pruneNames", "prunePaths", "pruneFS"]
                          let fs   = [return . mkPrune isInfixOf, return . mkPrune (==), isFs]
                          fs <- sequenceA . catMaybes $ zipWith fmap fs vars
                          return (\x -> any ($ x) fs)
          Left  err -> hPrint stderr err >>
                       hPutStrLn stderr "\nContinuing without pruning..." >>
                       return (const False)

isFs :: [String] -> IO (FilePath -> Bool)
isFs fs = flip elem . map mnt_dir . filter ((`elem` fs) . mnt_fsname) <$> getMounts

mkPrune :: (a -> a -> Bool) -> [a] -> a -> Bool
mkPrune f xs y = any (`f` y) xs
