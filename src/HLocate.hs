module Main where

import HLocate.Options (Opts (..), parseOpts)
import Shared.File (File (..))

import Control.Monad (liftM2)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Lens (view)
import Pipes
import Pipes.Binary (decoded)
import System.IO (withFile, IOMode (..), Handle)

import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as P

main :: IO ()
main = parseOpts >>= runReaderT queryDB

queryDB :: ReaderT Opts IO ()
queryDB = do loc <- asks location
             ao  <- asks andOr
             qs  <- liftM2 (.) (asks matchFunc) (asks baseName)  >>= (\f -> map f <$> asks queries)
             ec  <- asks endChar
             liftIO . withFile loc ReadMode $ \h -> runEffect $ 
                 for (decoder (PB.fromHandle h) 
                     >-> reconstruct 
                     >-> P.filter (\x -> ao (($ x) <$> qs)))
                 (lift . (\x -> putStr x >> putChar ec))

-- Convert stream of bytes into stream of decoded values, skipping errors
decoder :: Producer PB.ByteString IO () -> Producer File IO ()
decoder = void . view decoded

-- Convert File back into full filepath
reconstruct :: Pipe File FilePath IO ()
reconstruct = re ""
    where re fp = do f <- await
                     let new = take (prec f) fp ++ partial f
                      in yield new >> re new
