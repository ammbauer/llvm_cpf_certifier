module Main where

import LLVM_Checker


main = getContents >>= return . llvm_termination_checker >>= Prelude.putStrLn
