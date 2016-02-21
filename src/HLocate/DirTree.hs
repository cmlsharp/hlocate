{-# LANGUAGE DeriveGeneric #-}

module HLocate.DirTree (DirTree (..)) where

import Control.Exception (IOException)
import Data.Binary (Binary)
import Data.Traversable (foldMapDefault, fmapDefault)
import GHC.Generics (Generic)

data DirTree a = Node { name  :: a
                      , files :: [a]
                      , dirs  :: [DirTree a]
                      } 
               | Fail { name :: a
                      , msg :: String
                      } deriving (Generic, Show)


instance Binary a => Binary (DirTree a)

instance Functor DirTree where 
    fmap = fmapDefault

instance Foldable DirTree where
    foldMap = foldMapDefault

instance Traversable DirTree where
    traverse f (Node n fi ds) = Node <$> f n 
                                     <*> traverse f fi 
                                     <*> traverse (traverse f) ds 
    traverse f (Fail n m) = Fail <$> f n <*> pure m
