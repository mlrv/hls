{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.Foldable       as F
import           Data.Traversable    as T
import           Options.Applicative as O
import           System.Directory    as D

import           Models

main :: IO ()
main = do
  opts <- O.execParser cliParserInfo
  D.setCurrentDirectory (_path opts)
  files <- D.listDirectory "."
  files' <- elaborateFiles files

  let printer = case _long opts of
                     True -> putFileInfoLn
                     False -> putStrLn . _filename

  F.for_ files' printer

elaborateFiles :: [FilePath] -> IO [FileInfo]
elaborateFiles files =
  T.for files $ \file ->
    FileInfo file <$> D.getFileSize file <*> D.doesDirectoryExist file

putFileInfoLn :: FileInfo -> IO ()
putFileInfoLn = putStrLn . renderFileInfo

renderFileInfo :: FileInfo -> String
renderFileInfo file
  | _isdir file = _filename file ++ " dir"
renderFileInfo file
  | _size file >= 1024 =
    _filename file ++ " " ++ show (_size file `div` 1024) ++ "KB"
renderFileInfo file = _filename file ++ " " ++ show (_size file) ++ "B"

prettySize :: Integer -> String
prettySize (prefixify -> (s, p)) = show s ++ " " ++ p ++ "B"

prefixify :: Integer -> (Integer, String)
prefixify s
  | s >= 1024 = (s `div` 1024, "K")
prefixify s = (s, "")

cliParser :: O.Parser Options
cliParser =
	Options <$> O.strArgument (O.metavar "PATH" <> O.value ".")
	        <*> O.switch (long "long" <> short 'l' <> help "Enable long output")

cliParserInfo :: O.ParserInfo Options
cliParserInfo = O.info (O.helper <*> cliParser) mempty
