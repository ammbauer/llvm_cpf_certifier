{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Arith(Int(..), equal_int, times_int, Times(..), Dvd, Num(..), one_int,
         One(..), uminus_int, minus_int, zero_int, plus_int, Uminus(..),
         Minus(..), Zero(..), Plus(..), Semigroup_add, Cancel_semigroup_add,
         Ab_semigroup_add, Cancel_ab_semigroup_add, Monoid_add, Comm_monoid_add,
         Cancel_comm_monoid_add, Mult_zero, Semigroup_mult, Semiring,
         Semiring_0, Semiring_0_cancel, Group_add, Ab_group_add, Ring, Numeral,
         Power, Monoid_mult, Semiring_numeral, Zero_neq_one, Semiring_1,
         Semiring_1_cancel, Neg_numeral, Ring_1, less_eq_int, less_int,
         Semiring_char_0, Ab_semigroup_mult, Comm_semiring, Comm_semiring_0,
         Comm_monoid_mult, Comm_semiring_1, Nat, integer_of_nat, equal_nat,
         one_nat, zero_nat, less_eq_nat, less_nat, Char(..), equal_char,
         Semiring_no_zero_divisors, Semiring_1_no_zero_divisors,
         Comm_semiring_0_cancel, Comm_semiring_1_cancel, Semidom,
         Comm_semiring_1_cancel_crossproduct, Semiring_no_zero_divisors_cancel,
         Ring_no_zero_divisors, Ring_1_no_zero_divisors, Comm_ring, Comm_ring_1,
         Idom, Divide(..), Semidom_divide, Idom_divide, Ufd, Inverse(..),
         Division_ring, Field, Abs(..), Abs_if, Ring_char_0, Sgn(..),
         Idom_abs_sgn, Ordered_ab_semigroup_add,
         Strict_ordered_ab_semigroup_add, Ordered_cancel_ab_semigroup_add,
         Ordered_comm_monoid_add, Ordered_semiring, Ordered_semiring_0,
         Ordered_cancel_semiring, Ordered_ab_semigroup_add_imp_le,
         Strict_ordered_comm_monoid_add, Ordered_cancel_comm_monoid_add,
         Ordered_ab_semigroup_monoid_add_imp_le, Ordered_ab_group_add,
         Ordered_ring, Field_char_0, Zero_less_one, Field_abs_sgn,
         Linordered_ab_semigroup_add, Linordered_cancel_ab_semigroup_add,
         Linordered_semiring, Linordered_semiring_strict, Linordered_semiring_1,
         Linordered_semiring_1_strict, Ordered_ab_group_add_abs,
         Linordered_ab_group_add, Linordered_ring, Linordered_ring_strict,
         Ordered_semiring_1, Ordered_semiring_strict, Semiring_real_line,
         Semiring_1_real_line, Ordered_comm_semiring,
         Ordered_cancel_comm_semiring, Linordered_comm_semiring_strict,
         Linordered_nonzero_semiring, Linordered_semidom, Ordered_comm_ring,
         Ordered_ring_abs, Linordered_idom, Linordered_field, nat, plus_nat,
         suc, implode, nat_of_integer, divmod_nat, char_of_integer, explode,
         abs_int, sgn_int, int_of_nat, nat_of_char, of_nat, minus_nat,
         times_nat, divide_integer, divide_int, divide_nat, modulo_nat)
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
import qualified Product_Type;
import qualified HOL;
import qualified Orders;

newtype Int = Int_of_integer Integer;

integer_of_int :: Int -> Integer;
integer_of_int (Int_of_integer k) = k;

equal_int :: Int -> Int -> Bool;
equal_int k l = integer_of_int k == integer_of_int l;

instance Eq Int where {
  a == b = equal_int a b;
};

times_int :: Int -> Int -> Int;
times_int k l = Int_of_integer (integer_of_int k * integer_of_int l);

class Times a where {
  times :: a -> a -> a;
};

class (Times a) => Dvd a where {
};

instance Times Int where {
  times = times_int;
};

instance Dvd Int where {
};

data Num = One | Bit0 Num | Bit1 Num;

one_int :: Int;
one_int = Int_of_integer (1 :: Integer);

class One a where {
  one :: a;
};

instance One Int where {
  one = one_int;
};

uminus_int :: Int -> Int;
uminus_int k = Int_of_integer (negate (integer_of_int k));

minus_int :: Int -> Int -> Int;
minus_int k l = Int_of_integer (integer_of_int k - integer_of_int l);

zero_int :: Int;
zero_int = Int_of_integer (0 :: Integer);

plus_int :: Int -> Int -> Int;
plus_int k l = Int_of_integer (integer_of_int k + integer_of_int l);

class Uminus a where {
  uminus :: a -> a;
};

class Minus a where {
  minus :: a -> a -> a;
};

class Zero a where {
  zero :: a;
};

class Plus a where {
  plus :: a -> a -> a;
};

class (Plus a) => Semigroup_add a where {
};

class (Semigroup_add a) => Cancel_semigroup_add a where {
};

class (Semigroup_add a) => Ab_semigroup_add a where {
};

class (Ab_semigroup_add a, Cancel_semigroup_add a,
        Minus a) => Cancel_ab_semigroup_add a where {
};

class (Semigroup_add a, Zero a) => Monoid_add a where {
};

class (Ab_semigroup_add a, Monoid_add a) => Comm_monoid_add a where {
};

class (Cancel_ab_semigroup_add a,
        Comm_monoid_add a) => Cancel_comm_monoid_add a where {
};

class (Times a, Zero a) => Mult_zero a where {
};

class (Times a) => Semigroup_mult a where {
};

class (Ab_semigroup_add a, Semigroup_mult a) => Semiring a where {
};

class (Comm_monoid_add a, Mult_zero a, Semiring a) => Semiring_0 a where {
};

class (Cancel_comm_monoid_add a, Semiring_0 a) => Semiring_0_cancel a where {
};

class (Cancel_semigroup_add a, Minus a, Monoid_add a,
        Uminus a) => Group_add a where {
};

class (Cancel_comm_monoid_add a, Group_add a) => Ab_group_add a where {
};

class (Ab_group_add a, Semiring_0_cancel a) => Ring a where {
};

instance Plus Int where {
  plus = plus_int;
};

instance Semigroup_add Int where {
};

instance Cancel_semigroup_add Int where {
};

instance Ab_semigroup_add Int where {
};

instance Minus Int where {
  minus = minus_int;
};

instance Cancel_ab_semigroup_add Int where {
};

instance Zero Int where {
  zero = zero_int;
};

instance Monoid_add Int where {
};

instance Comm_monoid_add Int where {
};

instance Cancel_comm_monoid_add Int where {
};

instance Mult_zero Int where {
};

instance Semigroup_mult Int where {
};

instance Semiring Int where {
};

instance Semiring_0 Int where {
};

instance Semiring_0_cancel Int where {
};

instance Uminus Int where {
  uminus = uminus_int;
};

instance Group_add Int where {
};

instance Ab_group_add Int where {
};

instance Ring Int where {
};

class (One a, Semigroup_add a) => Numeral a where {
};

instance Numeral Int where {
};

class (One a, Times a) => Power a where {
};

instance Power Int where {
};

class (Semigroup_mult a, Power a) => Monoid_mult a where {
};

class (Monoid_mult a, Numeral a, Semiring a) => Semiring_numeral a where {
};

class (One a, Zero a) => Zero_neq_one a where {
};

class (Semiring_numeral a, Semiring_0 a, Zero_neq_one a) => Semiring_1 a where {
};

class (Semiring_0_cancel a, Semiring_1 a) => Semiring_1_cancel a where {
};

class (Group_add a, Numeral a) => Neg_numeral a where {
};

class (Neg_numeral a, Ring a, Semiring_1_cancel a) => Ring_1 a where {
};

instance Monoid_mult Int where {
};

instance Semiring_numeral Int where {
};

instance Zero_neq_one Int where {
};

instance Semiring_1 Int where {
};

instance Semiring_1_cancel Int where {
};

instance Neg_numeral Int where {
};

instance Ring_1 Int where {
};

less_eq_int :: Int -> Int -> Bool;
less_eq_int k l = integer_of_int k <= integer_of_int l;

less_int :: Int -> Int -> Bool;
less_int k l = integer_of_int k < integer_of_int l;

instance Orders.Ord Int where {
  less_eq = less_eq_int;
  less = less_int;
};

instance Orders.Quasi_order Int where {
};

instance Orders.Weak_order Int where {
};

instance Orders.Preorder Int where {
};

instance Orders.Order Int where {
};

instance Orders.Linorder Int where {
};

class (Semiring_1 a) => Semiring_char_0 a where {
};

instance Semiring_char_0 Int where {
};

class (Semigroup_mult a) => Ab_semigroup_mult a where {
};

class (Ab_semigroup_mult a, Semiring a) => Comm_semiring a where {
};

instance Ab_semigroup_mult Int where {
};

instance Comm_semiring Int where {
};

class (Comm_semiring a, Semiring_0 a) => Comm_semiring_0 a where {
};

instance Comm_semiring_0 Int where {
};

class (Ab_semigroup_mult a, Monoid_mult a, Dvd a) => Comm_monoid_mult a where {
};

class (Comm_monoid_mult a, Comm_semiring_0 a,
        Semiring_1 a) => Comm_semiring_1 a where {
};

instance Comm_monoid_mult Int where {
};

instance Comm_semiring_1 Int where {
};

newtype Nat = Nat Integer;

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

instance Eq Nat where {
  a == b = equal_nat a b;
};

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

instance One Nat where {
  one = one_nat;
};

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

instance Zero Nat where {
  zero = zero_nat;
};

less_eq_nat :: Nat -> Nat -> Bool;
less_eq_nat m n = integer_of_nat m <= integer_of_nat n;

less_nat :: Nat -> Nat -> Bool;
less_nat m n = integer_of_nat m < integer_of_nat n;

instance Orders.Ord Nat where {
  less_eq = less_eq_nat;
  less = less_nat;
};

instance Orders.Quasi_order Nat where {
};

instance Orders.Weak_order Nat where {
};

instance Orders.Preorder Nat where {
};

instance Orders.Order Nat where {
};

instance Orders.Linorder Nat where {
};

data Char = Char Bool Bool Bool Bool Bool Bool Bool Bool;

equal_char :: Char -> Char -> Bool;
equal_char (Char x1 x2 x3 x4 x5 x6 x7 x8) (Char y1 y2 y3 y4 y5 y6 y7 y8) =
  x1 == y1 &&
    x2 == y2 &&
      x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6 && x7 == y7 && x8 == y8;

instance Eq Char where {
  a == b = equal_char a b;
};

one_integer :: Integer;
one_integer = (1 :: Integer);

instance One Integer where {
  one = one_integer;
};

instance Zero Integer where {
  zero = (0 :: Integer);
};

instance Orders.Ord Integer where {
  less_eq = (\ a b -> a <= b);
  less = (\ a b -> a < b);
};

instance Zero_neq_one Integer where {
};

class (Semiring_0 a) => Semiring_no_zero_divisors a where {
};

class (Semiring_1 a,
        Semiring_no_zero_divisors a) => Semiring_1_no_zero_divisors a where {
};

class (Comm_semiring_0 a,
        Semiring_0_cancel a) => Comm_semiring_0_cancel a where {
};

class (Comm_semiring_0_cancel a, Comm_semiring_1 a,
        Semiring_1_cancel a) => Comm_semiring_1_cancel a where {
};

class (Comm_semiring_1_cancel a,
        Semiring_1_no_zero_divisors a) => Semidom a where {
};

class (Comm_semiring_1_cancel a) => Comm_semiring_1_cancel_crossproduct a where {
};

class (Semiring_no_zero_divisors a) => Semiring_no_zero_divisors_cancel a where {
};

class (Ring a,
        Semiring_no_zero_divisors_cancel a) => Ring_no_zero_divisors a where {
};

class (Ring_1 a, Ring_no_zero_divisors a,
        Semiring_1_no_zero_divisors a) => Ring_1_no_zero_divisors a where {
};

class (Comm_semiring_0_cancel a, Ring a) => Comm_ring a where {
};

class (Comm_ring a, Comm_semiring_1_cancel a, Ring_1 a) => Comm_ring_1 a where {
};

class (Comm_ring_1 a, Ring_1_no_zero_divisors a, Semidom a,
        Comm_semiring_1_cancel_crossproduct a) => Idom a where {
};

class Divide a where {
  divide :: a -> a -> a;
};

class (Divide a, Semidom a,
        Semiring_no_zero_divisors_cancel a) => Semidom_divide a where {
};

class (Idom a, Semidom_divide a) => Idom_divide a where {
};

class (Idom a) => Ufd a where {
};

class (Divide a) => Inverse a where {
  inverse :: a -> a;
};

class (Inverse a, Ring_1_no_zero_divisors a) => Division_ring a where {
};

class (Division_ring a, Idom_divide a, Ufd a) => Field a where {
};

class Abs a where {
  absa :: a -> a;
};

class (Abs a, Minus a, Uminus a, Zero a, Orders.Ord a) => Abs_if a where {
};

class (Semiring_char_0 a, Ring_1 a) => Ring_char_0 a where {
};

class Sgn a where {
  sgn :: a -> a;
};

class (Abs a, Sgn a, Idom a) => Idom_abs_sgn a where {
};

class (Ab_semigroup_add a, Orders.Order a) => Ordered_ab_semigroup_add a where {
};

class (Ordered_ab_semigroup_add a) => Strict_ordered_ab_semigroup_add a where {
};

class (Cancel_ab_semigroup_add a,
        Strict_ordered_ab_semigroup_add a) => Ordered_cancel_ab_semigroup_add a where {
};

class (Comm_monoid_add a,
        Ordered_ab_semigroup_add a) => Ordered_comm_monoid_add a where {
};

class (Ordered_comm_monoid_add a, Semiring a) => Ordered_semiring a where {
};

class (Ordered_semiring a, Semiring_0 a) => Ordered_semiring_0 a where {
};

class (Ordered_cancel_ab_semigroup_add a, Ordered_semiring_0 a,
        Semiring_0_cancel a) => Ordered_cancel_semiring a where {
};

class (Ordered_cancel_ab_semigroup_add a) => Ordered_ab_semigroup_add_imp_le a where {
};

class (Comm_monoid_add a,
        Strict_ordered_ab_semigroup_add a) => Strict_ordered_comm_monoid_add a where {
};

class (Ordered_cancel_ab_semigroup_add a, Ordered_comm_monoid_add a,
        Strict_ordered_comm_monoid_add a) => Ordered_cancel_comm_monoid_add a where {
};

class (Cancel_comm_monoid_add a, Ordered_ab_semigroup_add_imp_le a,
        Ordered_cancel_comm_monoid_add a) => Ordered_ab_semigroup_monoid_add_imp_le a where {
};

class (Ab_group_add a,
        Ordered_ab_semigroup_monoid_add_imp_le a) => Ordered_ab_group_add a where {
};

class (Ordered_ab_group_add a, Ordered_cancel_semiring a,
        Ring a) => Ordered_ring a where {
};

class (Field a, Ring_char_0 a) => Field_char_0 a where {
};

class (Orders.Order a, Zero_neq_one a) => Zero_less_one a where {
};

class (Field a, Idom_abs_sgn a) => Field_abs_sgn a where {
};

class (Ordered_ab_semigroup_add a,
        Orders.Linorder a) => Linordered_ab_semigroup_add a where {
};

class (Linordered_ab_semigroup_add a,
        Ordered_ab_semigroup_add_imp_le a) => Linordered_cancel_ab_semigroup_add a where {
};

class (Linordered_cancel_ab_semigroup_add a,
        Ordered_ab_semigroup_monoid_add_imp_le a,
        Ordered_cancel_semiring a) => Linordered_semiring a where {
};

class (Linordered_semiring a) => Linordered_semiring_strict a where {
};

class (Linordered_semiring a, Semiring_1 a,
        Zero_less_one a) => Linordered_semiring_1 a where {
};

class (Linordered_semiring_1 a,
        Linordered_semiring_strict a) => Linordered_semiring_1_strict a where {
};

class (Abs a, Ordered_ab_group_add a) => Ordered_ab_group_add_abs a where {
};

class (Linordered_cancel_ab_semigroup_add a,
        Ordered_ab_group_add a) => Linordered_ab_group_add a where {
};

class (Linordered_ab_group_add a, Ordered_ab_group_add_abs a, Abs_if a,
        Linordered_semiring a, Ordered_ring a) => Linordered_ring a where {
};

class (Linordered_ring a, Linordered_semiring_strict a,
        Ring_no_zero_divisors a) => Linordered_ring_strict a where {
};

class (Ordered_semiring_0 a, Semiring_1 a,
        Zero_less_one a) => Ordered_semiring_1 a where {
};

class (Ordered_cancel_ab_semigroup_add a, Ordered_semiring a,
        Semiring_0_cancel a) => Ordered_semiring_strict a where {
};

class (Ordered_semiring_strict a,
        Ordered_semiring_0 a) => Semiring_real_line a where {
};

class (Ordered_semiring_1 a,
        Semiring_real_line a) => Semiring_1_real_line a where {
};

class (Comm_semiring_0 a, Ordered_semiring a) => Ordered_comm_semiring a where {
};

class (Comm_semiring_0_cancel a, Ordered_cancel_semiring a,
        Ordered_comm_semiring a) => Ordered_cancel_comm_semiring a where {
};

class (Linordered_semiring_strict a,
        Ordered_cancel_comm_semiring a) => Linordered_comm_semiring_strict a where {
};

class (Semiring_char_0 a, Orders.Linorder a, Comm_semiring_1 a,
        Ordered_comm_semiring a,
        Zero_less_one a) => Linordered_nonzero_semiring a where {
};

class (Linordered_comm_semiring_strict a, Linordered_nonzero_semiring a,
        Semidom a) => Linordered_semidom a where {
};

class (Comm_ring a, Ordered_cancel_comm_semiring a,
        Ordered_ring a) => Ordered_comm_ring a where {
};

class (Ordered_ab_group_add_abs a, Ordered_ring a) => Ordered_ring_abs a where {
};

class (Ring_char_0 a, Semiring_1_real_line a, Idom_abs_sgn a,
        Linordered_ring_strict a, Linordered_semidom a,
        Linordered_semiring_1_strict a, Ordered_comm_ring a,
        Ordered_ring_abs a) => Linordered_idom a where {
};

class (Field_abs_sgn a, Field_char_0 a, Orders.Unbounded_dense_linorder a,
        Linordered_idom a) => Linordered_field a where {
};

nat :: Int -> Nat;
nat k = Nat (Orders.max (0 :: Integer) (integer_of_int k));

plus_nat :: Nat -> Nat -> Nat;
plus_nat m n = Nat (integer_of_nat m + integer_of_nat n);

suc :: Nat -> Nat;
suc n = plus_nat n one_nat;

of_bool :: forall a. (Zero_neq_one a) => Bool -> a;
of_bool True = one;
of_bool False = zero;

integer_of_char :: Char -> Integer;
integer_of_char (Char b0 b1 b2 b3 b4 b5 b6 b7) =
  ((((((of_bool b7 * (2 :: Integer) + of_bool b6) * (2 :: Integer) +
        of_bool b5) *
        (2 :: Integer) +
       of_bool b4) *
       (2 :: Integer) +
      of_bool b3) *
      (2 :: Integer) +
     of_bool b2) *
     (2 :: Integer) +
    of_bool b1) *
    (2 :: Integer) +
    of_bool b0;

implode :: [Char] -> String;
implode cs =
  map (let chr k | (0 <= k && k < 128) = Prelude.toEnum k :: Prelude.Char in chr . Prelude.fromInteger)
    (map integer_of_char cs);

nat_of_integer :: Integer -> Nat;
nat_of_integer k = Nat (Orders.max (0 :: Integer) k);

divmod_nat :: Nat -> Nat -> (Nat, Nat);
divmod_nat m n =
  let {
    k = integer_of_nat m;
    l = integer_of_nat n;
  } in Product_Type.map_prod nat_of_integer nat_of_integer
         (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else divMod ( k ) ( l )));

bit_cut_integer :: Integer -> (Integer, Bool);
bit_cut_integer k =
  (if k == (0 :: Integer) then ((0 :: Integer), False)
    else (case divMod (abs k) (abs (2 :: Integer)) of {
           (r, s) ->
             ((if (0 :: Integer) < k then r else negate r - s),
               s == (1 :: Integer));
         }));

char_of_integer :: Integer -> Char;
char_of_integer k =
  (case bit_cut_integer k of {
    (q0, b0) ->
      (case bit_cut_integer q0 of {
        (q1, b1) ->
          (case bit_cut_integer q1 of {
            (q2, b2) ->
              (case bit_cut_integer q2 of {
                (q3, b3) ->
                  (case bit_cut_integer q3 of {
                    (q4, b4) ->
                      (case bit_cut_integer q4 of {
                        (q5, b5) ->
                          (case bit_cut_integer q5 of {
                            (q6, b6) ->
                              let {
                                a = bit_cut_integer q6;
                              } in (case a of {
                                     (_, aa) -> Char b0 b1 b2 b3 b4 b5 b6 aa;
                                   });
                          });
                      });
                  });
              });
          });
      });
  });

explode :: String -> [Char];
explode s =
  map char_of_integer
    (map (let ord k | (k < 128) = Prelude.toInteger k in ord . (Prelude.fromEnum :: Prelude.Char -> Prelude.Int))
      s);

abs_int :: Int -> Int;
abs_int i = (if less_int i zero_int then uminus_int i else i);

sgn_int :: Int -> Int;
sgn_int i =
  (if equal_int i zero_int then zero_int
    else (if less_int zero_int i then one_int else uminus_int one_int));

numeral :: forall a. (Numeral a) => Num -> a;
numeral (Bit1 n) = let {
                     m = numeral n;
                   } in plus (plus m m) one;
numeral (Bit0 n) = let {
                     m = numeral n;
                   } in plus m m;
numeral One = one;

int_of_nat :: Nat -> Int;
int_of_nat n = Int_of_integer (integer_of_nat n);

divmod_integer :: Integer -> Integer -> (Integer, Integer);
divmod_integer k l =
  (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
    else (if (0 :: Integer) < l
           then (if (0 :: Integer) < k then divMod ( k ) ( l )
                  else (case divMod ( (negate k) ) ( l ) of {
                         (r, s) ->
                           (if s == (0 :: Integer)
                             then (negate r, (0 :: Integer))
                             else (negate r - (1 :: Integer), l - s));
                       }))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else Product_Type.apsnd negate
                         (if k < (0 :: Integer)
                           then divMod ( (negate k) ) ( (negate l) )
                           else (case divMod ( k ) ( (negate l) ) of {
                                  (r, s) ->
                                    (if s == (0 :: Integer)
                                      then (negate r, (0 :: Integer))
                                      else (negate r - (1 :: Integer),
     negate l - s));
                                })))));

nat_of_char :: Char -> Nat;
nat_of_char c = Nat (integer_of_char c);

of_nat :: forall a. (Semiring_1 a) => Nat -> a;
of_nat n =
  (if equal_nat n zero_nat then zero
    else (case divmod_nat n (nat_of_integer (2 :: Integer)) of {
           (m, q) -> let {
                       ma = times (numeral (Bit0 One)) (of_nat m);
                     } in (if equal_nat q zero_nat then ma else plus ma one);
         }));

minus_nat :: Nat -> Nat -> Nat;
minus_nat m n =
  Nat (Orders.max (0 :: Integer) (integer_of_nat m - integer_of_nat n));

times_nat :: Nat -> Nat -> Nat;
times_nat m n = Nat (integer_of_nat m * integer_of_nat n);

divide_integer :: Integer -> Integer -> Integer;
divide_integer k l = fst (divmod_integer k l);

divide_int :: Int -> Int -> Int;
divide_int k l =
  Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));

divide_nat :: Nat -> Nat -> Nat;
divide_nat m n = Nat (divide_integer (integer_of_nat m) (integer_of_nat n));

modulo_integer :: Integer -> Integer -> Integer;
modulo_integer k l = snd (divmod_integer k l);

modulo_nat :: Nat -> Nat -> Nat;
modulo_nat m n = Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));

}
