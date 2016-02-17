module RTree where

import Data.Traversable (fmapDefault, foldMapDefault)

data RTree a = a :> [RTree a] deriving Show

instance Functor RTree where
    fmap = fmapDefault

instance Foldable RTree where
    foldMap = foldMapDefault

instance Traversable RTree where
    traverse f (x:>xs) = (:>) <$> f x <*> traverse (traverse f) xs

