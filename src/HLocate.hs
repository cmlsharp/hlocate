module Main where


import HLocate.Options (Opts (..), parseOpts)
import Shared.File (File (..))

import Control.Lens (view)
import Data.List (isInfixOf)
import Pipes
import Pipes.Binary (decoded)
import System.IO (withFile, IOMode (..))

import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as P

main = parseOpts >>= handleOpts

handleOpts :: Opts -> IO ()
handleOpts op = queryDB (isInfixOf <$> queries op) (location op)


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
