{-# LANGUAGE OverloadedStrings #-}

module Main where

import Renamers
import Util

import Options.Applicative

data RunOptions = RunOptions { path :: String , start :: Int }

runoptions :: Parser RunOptions
runoptions = RunOptions
    <$> argument str (metavar "PATH" <> help "Path of the directory whose files will be renamed")
    <*> option auto (long "start" <> metavar "N" <> value 1 <> showDefault <> help "Number from which the sequence will start")

opts :: ParserInfo RunOptions
opts = info (runoptions <**> helper)
    ( fullDesc
    <> progDesc "Copy all files in a folder as <foldername><sequence_number>.<ext>"
    <> header "hrenamer - Auto mass renamer" )

main :: IO ()
main = do 
    options <- execParser opts
    renameFromFolderName (makeFilePath $ path options) (start options)
