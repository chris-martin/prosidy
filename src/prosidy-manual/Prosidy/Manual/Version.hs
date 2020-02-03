module Prosidy.Manual.Version where

import qualified Development.Shake as Shake
import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (lift)
import System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Data.Set as Set
import Data.Set (Set)

shakeVersionQ :: ExpQ
shakeVersionQ = lift =<< runIO calculateShakeVersion

calculateShakeVersion :: IO String
calculateShakeVersion = do
    files <- listDirectoryRecursive "src/prosidy-manual"
    print files
    Shake.getHashedShakeVersion (Set.toList files)

listDirectoryRecursive :: FilePath -> IO (Set FilePath)
listDirectoryRecursive dir = do
    contents <- Dir.listDirectory dir
    flip foldMap contents $ \fileSansDir -> do
        let file = dir </> fileSansDir
        isDir <- Dir.doesDirectoryExist file
        if isDir 
            then listDirectoryRecursive file 
            else pure (Set.singleton file)