module Main where

import LLVM_Checker
import LLVM_HS


main = do
  prog <- getContents
  putStrLn (run_prog_string prog "main")
