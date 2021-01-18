{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Option(bind, is_none, the, map_option, rel_option) where {

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

bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b;
bind Nothing f = Nothing;
bind (Just x) f = f x;

is_none :: forall a. Maybe a -> Bool;
is_none (Just x) = False;
is_none Nothing = True;

the :: forall a. Maybe a -> a;
the (Just x2) = x2;

map_option :: forall a b. (a -> b) -> Maybe a -> Maybe b;
map_option f Nothing = Nothing;
map_option f (Just x2) = Just (f x2);

rel_option :: forall a b. (a -> b -> Bool) -> Maybe a -> Maybe b -> Bool;
rel_option r Nothing (Just y2) = False;
rel_option r (Just y2) Nothing = False;
rel_option r Nothing Nothing = True;
rel_option r (Just x2) (Just y2) = r x2 y2;

}
