module Models where

import           Data.Traversable as T

data FileInfo =
  FileInfo
    { _filename :: FilePath
    , _size     :: Integer
    , _isdir    :: Bool
    }

data Options =
  Options
    { _path :: String
    , _long :: Bool
    }
