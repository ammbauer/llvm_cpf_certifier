module Main where

import LLVM_Checker


main = getContents >>= return . llvm_lts_represents_checker >>= Prelude.putStrLn
