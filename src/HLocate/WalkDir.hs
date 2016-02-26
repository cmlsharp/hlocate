{-# LANGUAGE LambdaCase #-}

module HLocate.WalkDir (walkDir, walkDirPrune) where

import HLocate.File (File (..))

import Control.Monad (when)
import Control.Exception (try, IOException)
import Data.List (partition)
import Pipes
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (isDirectory, getSymbolicLinkStatus)

type FilesDirs = ([FilePath],[FilePath])

walkDir :: FilePath -> Producer File IO ()
walkDir = walkDirPrune (const False)

walkDirPrune :: (FilePath -> Bool) -> FilePath -> Producer File IO ()
walkDirPrune f r = yield (File 0 r) >> getAll f r >-> createFile r

getAll :: (FilePath -> Bool) -> FilePath -> Producer String IO ()
getAll f r = liftIO (try (formatContents f r >>= filesAndDirs) :: IO (Either IOException FilesDirs)) >>= 
             \case Left  _       -> yield r
                   Right (fi, d) -> yield r >> for (each fi) yield >> for (each d) (getAll f)
          
createFile :: FilePath -> Pipe String File IO ()
createFile s = do fp <- await
                  let com = inCommon s fp
                  when (s /= fp) $ yield $ File com (drop com fp)
                  createFile fp

inCommon :: FilePath -> FilePath -> Int
inCommon x y = length . takeWhile ((==) <$> fst <*> snd) $ zip x y

formatContents :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
formatContents f d = filter (not .f) . fmap (d </>) . exceptLocal <$> getDirectoryContents d
    where exceptLocal = filter ((&&) <$> (/=) "." <*> (/=) "..")

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = traverse (fmap <$> (,) <*> isDir)
    where isDir = fmap isDirectory .  getSymbolicLinkStatus

filesAndDirs :: [FilePath] -> IO FilesDirs
filesAndDirs = fmap (bimap (map fst) . partition (not . snd)) . tagDirectories
    where bimap f (a,b) = (f a, f b)
