{-# LANGUAGE LambdaCase #-}

module HUpdate.WalkDir (walkDir, walkDirPrune) where

import Shared.File (File (..))

import Control.Monad (when)
import Control.Exception (try, IOException)
import Data.List (partition)
import Pipes
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

type FilesDirs = ([FilePath],[FilePath])

-- Like walkDirPrune but do no pruning
walkDir :: FilePath -> Producer File IO ()
walkDir = walkDirPrune (const False)

-- Take a pruning function and a root directory and return a Producer of Files
walkDirPrune :: (FilePath -> Bool) -> FilePath -> Producer File IO ()
walkDirPrune f r = yield (File 0 r) >> getAll f r >-> createFile r

-- Take a pruning function and root directory and return a producer yielding file paths
getAll :: (FilePath -> Bool) -> FilePath -> Producer FilePath IO ()
getAll f r = liftIO (try (formatContents f r >>= filesAndDirs) :: IO (Either IOException FilesDirs)) >>= 
             \case Left  _       -> yield r
                   Right (fi, d) -> yield r >> for (each fi) yield >> for (each d) (getAll f)
          
-- Convert filepaths to File data structure
createFile :: FilePath -> Pipe String File IO ()
createFile s = do fp <- await
                  let com = inCommon s fp
                  when (s /= fp) $ yield $ File com (drop com fp)
                  createFile fp

-- Determine how many characters are shared between two file paths
inCommon :: FilePath -> FilePath -> Int
inCommon x y = length . takeWhile ((==) <$> fst <*> snd) $ zip x y

-- Take a pruning function and a root directory and return the contents of that directory (filtering out . and ..)
formatContents :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
formatContents f d = filter (not .f) . fmap (d </>) . exceptLocal <$> getDirectoryContents d
    where exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")

-- Tag each file path as a file or directory (True for directories, False for files)
tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory .  getSymbolicLinkStatus

-- Seperate a list file paths into ([Files], [Directories])
filesAndDirs :: [FilePath] -> IO FilesDirs
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)
