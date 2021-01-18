{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Bits_Int(bitXOR_int) where {

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
import qualified Arith;

bitXOR_int :: Arith.Int -> Arith.Int -> Arith.Int;
bitXOR_int (Arith.Int_of_integer i) (Arith.Int_of_integer j) =
  Arith.Int_of_integer ((Data_Bits.xor :: Integer -> Integer -> Integer) i j);

}
