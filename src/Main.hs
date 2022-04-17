
module Main(main) where

import System.Environment
import System.IO
import Hoogle


main :: IO ()
main = do
    [source, destination] <- getArgs
    runEvaluation source destination
    return ()
    -- hSetEncoding stdout utf8
    -- hoogle =<< getArgs
