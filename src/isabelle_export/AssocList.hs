{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module AssocList(keys, delete) where {

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
import qualified DAList;
import qualified Impl;

keys ::
  forall a b.
    (Impl.Ceq a, Impl.Ccompare a,
      Impl.Set_impl a) => DAList.Alist a b -> Impl.Set a;
keys xa = Impl.set (map fst (DAList.impl_of xa));

delete_aux :: forall a b. (Eq a) => a -> [(a, b)] -> [(a, b)];
delete_aux k [] = [];
delete_aux ka ((k, v) : xs) =
  (if ka == k then xs else (k, v) : delete_aux ka xs);

delete :: forall a b. (Eq a) => a -> DAList.Alist a b -> DAList.Alist a b;
delete xb xc = DAList.Alist (delete_aux xb (DAList.impl_of xc));

}
