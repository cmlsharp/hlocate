{-# LANGUAGE DeriveGeneric #-}

module Shared.File where

import Pipes.Binary (Binary)
import GHC.Generics (Generic)

data File = File { prec :: Int -- Number of characters shared between current file and previous one
                 , partial :: String -- Characters after shared prefix
                 } deriving (Show, Generic)

instance Binary File
