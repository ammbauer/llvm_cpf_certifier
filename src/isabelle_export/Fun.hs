{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Fun(inj_on, fun_upd) where {

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
import qualified Impl;

inj_on ::
  forall a b.
    (Impl.Ceq a, Impl.Ccompare a, Eq a, Eq b) => (a -> b) -> Impl.Set a -> Bool;
inj_on f a =
  Impl.ball a
    (\ x -> Impl.ball a (\ y -> (if f x == f y then x == y else True)));

fun_upd :: forall a b. (Eq a) => (a -> b) -> a -> b -> a -> b;
fun_upd f a b = (\ x -> (if x == a then b else f x));

}
