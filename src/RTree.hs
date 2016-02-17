module RTree where

import qualified Data.Traversable as T

data RTree a = a :> [RTree a]

instance Show a => Show (RTree a) where
    show (a :> as) = show a ++ show as

instance Functor RTree where
    fmap = T.fmapDefault

instance Traversable RTree where
    traverse f (x:>xs) = (:>) <$> f x <*> traverse (traverse f) xs

instance Foldable RTree where
    foldMap = T.foldMapDefault
