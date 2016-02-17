module WalkDir {-(walkDir)-} where

import Data.List (partition)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

import DTree (DTree(Node))

walkDir :: FilePath -> IO (DTree FilePath)
walkDir r = formatContents r >>= filesAndDirs >>= uncurry (buildNode r)
    where buildNode r f d  = Node r f <$> traverse walkDir d

formatContents :: FilePath -> IO [FilePath]
formatContents d = fmap (d </>) . exceptLocal <$> getDirectoryContents d
    where exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory . getSymbolicLinkStatus

filesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)
