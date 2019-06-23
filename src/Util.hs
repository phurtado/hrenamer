module Util
    ( makeFilePath,
      padInt
    ) where

import qualified Turtle as Sh
import qualified Data.Text as T

makeFilePath :: String -> Sh.FilePath
makeFilePath = Sh.fromText . T.pack 

padInt :: Int -> Int -> T.Text
padInt n i = (T.pack $ replicate (n - length (show i)) '0') <> (T.pack $ show i)