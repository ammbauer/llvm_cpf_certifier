{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Orders(Ord(..), Preorder, Quasi_order, Weak_order, Order, Inf(..),
          Semilattice_inf, Sup(..), Quasi_order_sup, Quasi_semilattice_sup,
          Semilattice_sup, Lattice, No_bot, No_top, Linorder, Dense_order,
          Dense_linorder, Unbounded_dense_linorder, max, min)
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

class Ord a where {
  less_eq :: a -> a -> Bool;
  less :: a -> a -> Bool;
};

class (Ord a) => Preorder a where {
};

class (Ord a) => Quasi_order a where {
};

class (Quasi_order a) => Weak_order a where {
};

class (Preorder a, Weak_order a) => Order a where {
};

class Inf a where {
  inf :: a -> a -> a;
};

class (Inf a, Order a) => Semilattice_inf a where {
};

class Sup a where {
  sup :: a -> a -> a;
};

class (Sup a, Quasi_order a) => Quasi_order_sup a where {
};

class (Quasi_order_sup a) => Quasi_semilattice_sup a where {
};

class (Order a, Quasi_semilattice_sup a) => Semilattice_sup a where {
};

class (Semilattice_inf a, Semilattice_sup a) => Lattice a where {
};

class (Order a) => No_bot a where {
};

class (Order a) => No_top a where {
};

class (Order a) => Linorder a where {
};

class (Order a) => Dense_order a where {
};

class (Dense_order a, Linorder a) => Dense_linorder a where {
};

class (Dense_linorder a, No_bot a,
        No_top a) => Unbounded_dense_linorder a where {
};

max :: forall a. (Ord a) => a -> a -> a;
max a b = (if less_eq a b then b else a);

min :: forall a. (Ord a) => a -> a -> a;
min a b = (if less_eq a b then a else b);

}
