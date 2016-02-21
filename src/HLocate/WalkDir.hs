{-# LANGUAGE LambdaCase #-}

module HLocate.WalkDir
    ( walkDir
    , walkDirPrune
    , DirTree (..)
    ) where

import HLocate.DirTree (DirTree(..))

import Control.Exception (try, displayException, IOException)
import Data.List (partition)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

walkDir :: FilePath -> IO (DirTree FilePath)
walkDir = walkDirPrune (const False)

walkDirPrune :: (FilePath -> Bool) -> FilePath -> IO (DirTree FilePath)
walkDirPrune f r = try (formatContents f r >>= filesAndDirs) >>=
    \case Left e       -> return $ Fail r (displayException (e :: IOException)) 
          Right (fi,d) -> Node r fi <$> (traverse (walkDirPrune f) d)

formatContents :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
formatContents f d = filter (not .f) . fmap (d </>) . exceptLocal <$> getDirectoryContents d
    where exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory .  getSymbolicLinkStatus

filesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)
