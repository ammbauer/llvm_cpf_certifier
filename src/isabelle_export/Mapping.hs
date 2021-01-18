{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Mapping(Mapping_impla(..), mapping_impl_nat, Mapping_impl(..), Mapping, keys,
           mapping_empty, empty, delete, lookup, update, of_alist, tabulate,
           ordered_keys, lookup_default, equal_mapping)
  where {

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
import qualified HOL;
import qualified Orders;
import qualified Product_Type;
import qualified Fun;
import qualified Comparator;
import qualified Option;
import qualified AssocList;
import qualified Impl;
import qualified DAList;
import qualified Phantom_Type;
import qualified Arith;

data Mapping_impla = Mapping_Choose | Mapping_Assoc_List | Mapping_RBT
  | Mapping_Mapping;

mapping_impl_nat :: Phantom_Type.Phantom Arith.Nat Mapping_impla;
mapping_impl_nat = Phantom_Type.Phantom Mapping_RBT;

class Mapping_impl a where {
  mapping_impl :: Phantom_Type.Phantom a Mapping_impla;
};

instance Mapping_impl Arith.Nat where {
  mapping_impl = mapping_impl_nat;
};

mapping_impl_list :: forall a. Phantom_Type.Phantom [a] Mapping_impla;
mapping_impl_list = Phantom_Type.Phantom Mapping_Choose;

instance Mapping_impl [a] where {
  mapping_impl = mapping_impl_list;
};

data Mapping a b = Assoc_List_Mapping (DAList.Alist a b)
  | RBT_Mapping (Impl.Mapping_rbt a b) | Mapping (a -> Maybe b);

keys ::
  forall a b.
    (Impl.Cenum a, Impl.Ceq a, Impl.Ccompare a,
      Impl.Set_impl a) => Mapping a b -> Impl.Set a;
keys (RBT_Mapping t) = Impl.RBT_set (Impl.mapb (\ _ _ -> ()) t);
keys (Assoc_List_Mapping al) = AssocList.keys al;
keys (Mapping m) = Impl.collect (\ k -> not (Option.is_none (m k)));

mapping_empty_choose :: forall a b. (Impl.Ccompare a) => Mapping a b;
mapping_empty_choose =
  (case (Impl.ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing -> Assoc_List_Mapping DAList.empty;
    Just _ -> RBT_Mapping Impl.emptya;
  });

mapping_empty :: forall a b. (Impl.Ccompare a) => Mapping_impla -> Mapping a b;
mapping_empty Mapping_RBT = RBT_Mapping Impl.emptya;
mapping_empty Mapping_Assoc_List = Assoc_List_Mapping DAList.empty;
mapping_empty Mapping_Mapping = Mapping (\ _ -> Nothing);
mapping_empty Mapping_Choose = mapping_empty_choose;

empty :: forall a b. (Impl.Ccompare a, Mapping_impl a) => Mapping a b;
empty =
  mapping_empty
    (Phantom_Type.of_phantom
      (mapping_impl :: Phantom_Type.Phantom a Mapping_impla));

delete ::
  forall a b. (Impl.Ccompare a, Eq a) => a -> Mapping a b -> Mapping a b;
delete k (RBT_Mapping t) =
  (case (Impl.ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "delete RBT_Mapping: ccompare = None" (\ _ -> delete k (RBT_Mapping t));
    Just _ -> RBT_Mapping (Impl.delete k t);
  });
delete k (Assoc_List_Mapping al) = Assoc_List_Mapping (AssocList.delete k al);
delete k (Mapping m) = Mapping (Fun.fun_upd m k Nothing);

lookup :: forall a b. (Impl.Ccompare a, Eq a) => Mapping a b -> a -> Maybe b;
lookup (RBT_Mapping t) = Impl.lookup t;
lookup (Assoc_List_Mapping al) = DAList.lookup al;

update ::
  forall a b. (Impl.Ccompare a, Eq a) => a -> b -> Mapping a b -> Mapping a b;
update k v (RBT_Mapping t) =
  (case (Impl.ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "update RBT_Mapping: ccompare = None"
        (\ _ -> update k v (RBT_Mapping t));
    Just _ -> RBT_Mapping (Impl.insertb k v t);
  });
update k v (Assoc_List_Mapping al) = Assoc_List_Mapping (DAList.update k v al);
update k v (Mapping m) = Mapping (Fun.fun_upd m k (Just v));

of_alist ::
  forall a b.
    (Impl.Ccompare a, Eq a, Mapping_impl a) => [(a, b)] -> Mapping a b;
of_alist xs = Impl.foldr (\ (a, b) -> update a b) xs empty;

tabulate ::
  forall a b.
    (Impl.Ccompare a, Eq a, Mapping_impl a) => [a] -> (a -> b) -> Mapping a b;
tabulate xs f = Impl.fold (\ k -> update k (f k)) xs empty;

ordered_keys ::
  forall a b.
    (Impl.Cenum a, Impl.Ceq a, Impl.Ccompare a, Orders.Linorder a,
      Impl.Set_impl a) => Mapping a b -> [a];
ordered_keys = Impl.sorted_list_of_set . keys;

lookup_default ::
  forall a b. (Impl.Ccompare b, Eq b) => a -> Mapping b a -> b -> a;
lookup_default d m k = (case lookup m k of {
                         Nothing -> d;
                         Just v -> v;
                       });

equal_mapping ::
  forall a b.
    (Impl.Cenum a, Impl.Ceq a, Impl.Ccompare a, Eq a, Impl.Set_impl a,
      Eq b) => Mapping a b -> Mapping a b -> Bool;
equal_mapping =
  (\ m ma ->
    let {
      k = keys m;
    } in Impl.set_eq k (keys ma) &&
           Impl.ball k (\ x -> lookup m x == lookup ma x));

}
