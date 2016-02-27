{-# LANGUAGE DeriveGeneric #-}

module Shared.File where

import Pipes.Binary (Binary)
import GHC.Generics (Generic)

data File = File { prec :: Int, partial :: String } deriving (Show, Generic)

instance Binary File
