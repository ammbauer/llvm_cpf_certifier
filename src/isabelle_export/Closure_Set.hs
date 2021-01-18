{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Closure_Set(fun_upd) where {

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

fun_upd :: forall a b. (a -> a -> Bool) -> (a -> b) -> a -> b -> a -> b;
fun_upd equal f aa b a = (if equal aa a then b else f a);

}
