module WalkDir (walkDir) where

import Data.List (partition)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

import DTree (DTree(Node))

walkDir :: FilePath -> IO (DTree FilePath)
walkDir r = do
    (files, dirs) <- fmap (r </>) . exceptLocal <$> getDirectoryContents r >>= filesAndDirs
    Node r files <$> traverse walkDir dirs 

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory . getSymbolicLinkStatus

filesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)

exceptLocal :: [FilePath] -> [FilePath]
exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")
