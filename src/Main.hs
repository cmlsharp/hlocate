{-# LANGUAGE LambdaCase #-}

module Main where

import HLocate.WalkDir
import HLocate.Options (Opts (..), parseOpts)

import Control.Monad.Trans.Reader 
import Control.Monad (when)
import Control.Lens (view)
import Data.List
import Pipes.Binary
import Pipes

import System.Environment
import System.IO

import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable   as F
import qualified Pipes.Prelude   as P
import qualified Pipes.ByteString as PB



-- TODO: Implement actual pruning
-- TODO: Proper error handling (maybe define monad combining ReaderT and ExceptT?)
-- TODO: Performance is terrible. Mess around with strictness/laziness. Maybe play around with Pipes for IO? 
-- TODO: Implement the rest of locate's features

main = parseOpts >>= runReaderT doStuff

doStuff :: ReaderT Opts IO ()
doStuff = do u <- asks update
             qs <- map isInfixOf <$> asks queries
             loc <- asks location
             if u then liftIO $ updateDB loc
                  else liftIO $ queryDB qs loc

updateDB :: FilePath -> IO ()
updateDB loc = withFile loc WriteMode encodeTree
    where encodeTree h = do tree <- liftIO $ walkDirPrune (isInfixOf ".snapshots") "/"
                            runEffect $ for (each tree) encode >-> PB.toHandle h

queryDB :: [FilePath -> Bool] -> FilePath -> IO ()
queryDB qs loc = withFile loc ReadMode printMatches
    where printMatches h = runEffect $ for (decoder (PB.fromHandle h)) (\x -> findMatches qs x >-> P.print)
          decoder p      = void (view decoded p)

findMatches :: [FilePath -> Bool] -> DirTree FilePath -> Producer FilePath IO ()
findMatches qs (Node n fi d) = testMatch qs n >> for (each fi) (testMatch qs) >> for (each d) (findMatches qs)
findMatches qs (Fail n _)    = testMatch qs n 
    
testMatch :: [FilePath -> Bool] -> FilePath -> Producer FilePath IO ()
{-# INLINE testMatch #-}
testMatch qs x = when (or (($ x) <$> qs)) (yield x)
