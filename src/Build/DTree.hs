{-# LANGUAGE DeriveGeneric #-}

module Build.DTree (DTree(..)) where

import Control.Exception (IOException)
import Data.Serialize (Serialize)
import Data.Traversable (foldMapDefault, fmapDefault)
import GHC.Generics (Generic)

data DTree a = Node { name  :: a
                    , files :: [a]
                    , dirs  :: [DTree a]
                    } 
             | Fail { name :: a
                    , msg :: String
                    } deriving (Generic, Show)


instance Serialize a =>  Serialize (DTree a)

instance Functor DTree where 
    fmap = fmapDefault

instance Foldable DTree where
    foldMap = foldMapDefault

instance Traversable DTree where
    traverse f (Node n fi ds) = Node <$> f n 
                                     <*> traverse f fi 
                                     <*> traverse (traverse f) ds 
    traverse f (Fail n m) = Fail <$> f n <*> pure m
