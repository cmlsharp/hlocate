{-# LANGUAGE BangPatterns #-}

module Main where

import HLocate.WalkDir
import HLocate.Options (Opts (..), parseOpts)

import Control.Monad.Trans.Reader 
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Data.List
import Data.Binary

import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable   as F

import System.Environment

-- TODO: Implement actual pruning
-- TODO: Proper error handling (maybe define monad combining ReaderT and ExceptT?)
-- TODO: Performance is terrible. Mess around with strictness/laziness. Maybe play around with Pipes for IO? 
-- TODO: Implement the rest of locate's features

main = parseOpts >>= runReaderT doStuff

doStuff :: ReaderT Opts IO ()
doStuff = do u <- asks update
             loc <- asks location
             qs <- map isInfixOf <$> asks queries
             liftIO $ if u then walkDirPrune (isInfixOf "/.snapshots") "/" >>= encodeFile loc
                           else do x <- decodeFile loc :: IO (DirTree FilePath)
                                   F.traverse_ (\x -> when (or (($ x) <$> qs)) $ print x) x

