{-# LANGUAGE LambdaCase #-}

module Build.WalkDir (walkDir) where

import Build.DTree (DTree(..))

import Control.Exception (try, displayException, IOException)
import Data.List (partition)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

walkDir :: FilePath -> IO (DTree FilePath)
walkDir r = try (formatContents r >>= filesAndDirs) >>=
    \case Left e      -> return $ Fail r (displayException (e :: IOException)) 
          Right (f,d) -> Node r f <$> (traverse walkDir d)

formatContents :: FilePath -> IO [FilePath]
formatContents d = fmap (d </>) . exceptLocal <$> getDirectoryContents d
    where exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory . getSymbolicLinkStatus

filesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)
