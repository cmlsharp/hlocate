module DTree (DTree(Node)) where

import Data.Traversable (foldMapDefault, fmapDefault)

data DTree a = Node { name  :: a
                    , files :: [a]
                    , dirs  :: [DTree a]
                    }

instance Functor DTree where 
    fmap = fmapDefault

instance Foldable DTree where
    foldMap = foldMapDefault

instance Traversable DTree where
    traverse f (Node n fs ds) = Node <$> f n <*> traverse f fs <*> traverse (traverse f) ds
