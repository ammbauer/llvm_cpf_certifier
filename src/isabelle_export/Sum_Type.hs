{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Sum_Type(Sum(..), map_sum) where {

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

data Sum a b = Inl a | Inr b;

map_sum :: forall a b c d. (a -> b) -> (c -> d) -> Sum a c -> Sum b d;
map_sum f1 f2 (Inl a) = Inl (f1 a);
map_sum f1 f2 (Inr a) = Inr (f2 a);

}
