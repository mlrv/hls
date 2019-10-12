module Main where

import           Data.Foldable    as F
import           Data.Traversable as T
import           System.Directory as D

import           Models

main :: IO ()
main = do
  files <- D.listDirectory "."
  files' <- elaborateFiles files
  F.for_ files' putFileInfoLn

elaborateFiles :: [FilePath] -> IO [FileInfo]
elaborateFiles files =
  T.for files $ \file ->
    FileInfo file <$> D.getFileSize file <*> D.doesDirectoryExist file

putFileInfoLn :: FileInfo -> IO ()
putFileInfoLn = putStrLn . renderFileInfo

renderFileInfo :: FileInfo -> String
renderFileInfo file = _filename file ++ ", " ++ show (_size file) ++ "B"
