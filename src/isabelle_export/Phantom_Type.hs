{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Phantom_Type(Phantom(..), of_phantom) where {

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

newtype Phantom a b = Phantom b;

of_phantom :: forall a b. Phantom a b -> b;
of_phantom (Phantom x) = x;

}
