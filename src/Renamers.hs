{-# LANGUAGE OverloadedStrings #-}

module Renamers
    ( renameFromFolderName,
      renameTest
    ) where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Util

data PathWithIndex = PathWithIndex {path :: FilePath, index :: Int}

-- |Main renamer
renameFromFolderName :: FilePath -> Int -> IO ()
renameFromFolderName fldr start = do
    isfldr <- testdir fldr
    if isfldr == True then do

        -- Append text to FilePath (How can this be so complicated!?)
        let targetNameAppend :: FilePath -> T.Text -> FilePath 
            targetNameAppend name app = fromText $ (format fp (basename name)) <> app

        -- Create new names from indexed old names
        let newName :: (FilePath, Int) -> (FilePath, FilePath)
            newName (oldName, idx) = (oldName, fldr </> (targetNameAppend fldr (padInt 5 idx)) <.> (maybe "" id (extension oldName)))

        {- Fold function that checks if the name without extension is already used to 
           keep files with the same name and different extension with the same root -}
        let fldFn :: [(FilePath, PathWithIndex)] -> FilePath -> [(FilePath, PathWithIndex)]
            fldFn acc x = case lookup (dropExtension x) (map (\y -> (dropExtension (fst y), snd y)) acc) of
                            Just p -> (x, PathWithIndex {path=(dropExtension $ path p) <.> (maybe "" id (extension x)), index=index p}) : acc
                            Nothing -> let nidx = 1 + (index $ snd $ head acc)
                                           (old, new) = newName (x, nidx)
                                       in (old, PathWithIndex {path=new, index=nidx}) : acc

        files <- (sort $ ls $ fldr)
                    >>= return . reverse . foldl fldFn [("", PathWithIndex {path="", index=0})]
                    >>= return . tail
                    >>= return . map (\(p, pwi) -> (p, path pwi))

        mapM_ (\(o, n) -> 
                    cp o n 
                >> (liftIO $ TIO.putStrLn ("Copied " <> format fp o <> " to " <> format fp n))
              ) 
              files
    else
        return ()

renameTest :: FilePath -> IO ()
renameTest name = do
    let targetFolder = parent name
    let targetName = fromText $ (format fp (basename name)) <> "2"
    let targetExt = maybe "" id (extension name)
    let target = targetFolder </> targetName <.> targetExt
    cp name target
    liftIO $ TIO.putStrLn ("Copied " <> format fp name <> " to " <> format fp target)