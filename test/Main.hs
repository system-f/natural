{-# OPTIONS_GHC -Wall -Werror -Wno-orphans #-}

module Main where

import System.Exit (exitWith)
import System.Process (rawSystem)

main :: IO ()
main =
  exitWith
    =<< rawSystem
      "cabal"
      [ "repl",
        "--with-compiler=doctest",
        "--repl-options=-w",
        "--repl-options=-Wdefault",
        "lib:natural"
      ]
