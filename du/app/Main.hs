module Main where

import System.Environment ( getArgs )
import Text.Read ( readMaybe )
import Du ( du )
import Data.Maybe ( isJust )

main :: IO ()
main = do
    args <- getArgs
    let argsLen = length args
    if argsLen == 1 then
        du (head args) Nothing
    else if argsLen == 2 then do
        let depthMb = readMaybe (args !! 1) :: Maybe Integer
        if isJust depthMb then
            du (head args) depthMb
        else
            putStrLn "Error: wrong depth"
    else
        putStrLn "Error: wrong args length"
