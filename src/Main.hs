module Main where

import HLocate.WalkDir (walkDirPrune)
import HLocate.Options (Opts (..), parseOpts)
import HLocate.File (File (..))

import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Lens (view)
import Data.List (isInfixOf)
import Pipes
import Pipes.Binary (decoded, encode)
import System.IO (withFile, IOMode (..))

import qualified Pipes.ByteString     as PB
import qualified Pipes.Prelude        as P



-- TODO: Implement actual pruning
-- TODO: Proper error handling (maybe define monad combining ReaderT and ExceptT?) (Pipes.Safe maybe?)
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
    where encodeTree h = runEffect $ for (walkDirPrune (isInfixOf ".snapshots") "/") encode >-> PB.toHandle h

queryDB :: [FilePath -> Bool] -> FilePath -> IO ()
queryDB qs loc = withFile loc ReadMode printMatches
    where decoder p      = void (view decoded p)
          printMatches h = runEffect $ decoder (PB.fromHandle h) 
              >-> reconstruct 
              >-> P.filter (\x -> or (($ x) <$> qs)) 
              >-> P.print

reconstruct :: Pipe File FilePath IO ()
reconstruct = re ""
    where re fp = do f <- await
                     let new = take (prec f) fp ++ partial f
                      in yield new >> re new
