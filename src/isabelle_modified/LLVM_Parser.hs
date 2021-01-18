{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module LLVM_Parser(parse_llvm) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Uint;
import qualified Array;
import qualified Uint32;
import qualified Uint64;
import qualified Data_Bits;
import qualified LLVM_Syntax;

import LLVM_AST;

parse_llvm :: String -> LLVM_Syntax.Llvm_prog;
parse_llvm = (\ a -> parse_llvm_string a);

}
