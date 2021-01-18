{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Shows_Literal(showsl_lit, showsl_nat, showsl_int, showsl_sep, showsl_list_gen,
                 default_showsl_list, showsl_list_int, Showl(..), showsl_option,
                 showsl_prod, showsl_list_prod, add_index, showsl_sum,
                 showsl_literal)
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
import qualified Sum_Type;
import qualified Arith;

lit_of_digit :: Arith.Nat -> String;
lit_of_digit n =
  Arith.implode
    [Arith.char_of_integer ((48 :: Integer) + Arith.integer_of_nat n)];

showsl_lit :: String -> String -> String;
showsl_lit = (\ a b -> a ++ b);

showsl_nat :: Arith.Nat -> String -> String;
showsl_nat n =
  (if Arith.less_nat n (Arith.nat_of_integer (10 :: Integer))
    then showsl_lit (lit_of_digit n)
    else showsl_nat
           (Arith.divide_nat n (Arith.nat_of_integer (10 :: Integer))) .
           showsl_lit
             (lit_of_digit
               (Arith.modulo_nat n (Arith.nat_of_integer (10 :: Integer)))));

showsl_int :: Arith.Int -> String -> String;
showsl_int i =
  (if Arith.less_int i Arith.zero_int
    then showsl_lit "-" . showsl_nat (Arith.nat (Arith.uminus_int i))
    else showsl_nat (Arith.nat i));

showsl_sep ::
  forall a.
    (a -> String -> String) -> (String -> String) -> [a] -> String -> String;
showsl_sep s sep [] = showsl_lit "";
showsl_sep s sep [x] = s x;
showsl_sep s sep (x : v : va) = (s x . sep) . showsl_sep s sep (v : va);

showsl_list_gen ::
  forall a.
    (a -> String -> String) ->
      String -> String -> String -> String -> [a] -> String -> String;
showsl_list_gen showslx e l s r xs =
  (if null xs then showsl_lit e
    else (showsl_lit l . showsl_sep showslx (showsl_lit s) xs) . showsl_lit r);

default_showsl_list ::
  forall a. (a -> String -> String) -> [a] -> String -> String;
default_showsl_list sl = showsl_list_gen sl "[]" "[" ", " "]";

showsl_list_int :: [Arith.Int] -> String -> String;
showsl_list_int xs = default_showsl_list showsl_int xs;

class Showl a where {
  showsl :: a -> String -> String;
  showsl_list :: [a] -> String -> String;
};

instance Showl Arith.Int where {
  showsl = showsl_int;
  showsl_list = showsl_list_int;
};

showsl_list_nat :: [Arith.Nat] -> String -> String;
showsl_list_nat xs = default_showsl_list showsl_nat xs;

instance Showl Arith.Nat where {
  showsl = showsl_nat;
  showsl_list = showsl_list_nat;
};

showsl_list_char :: [Arith.Char] -> String -> String;
showsl_list_char cs s = showsl_lit (Arith.implode cs) s;

showsl_char :: Arith.Char -> String -> String;
showsl_char c = showsl_lit (Arith.implode [c]);

instance Showl Arith.Char where {
  showsl = showsl_char;
  showsl_list = showsl_list_char;
};

showsl_option :: forall a. (Showl a) => Maybe a -> String -> String;
showsl_option Nothing = showsl_lit "None";
showsl_option (Just x) = (showsl_lit "Some (" . showsl x) . showsl_lit ")";

showsl_list_option :: forall a. (Showl a) => [Maybe a] -> String -> String;
showsl_list_option xs = default_showsl_list showsl_option xs;

instance (Showl a) => Showl (Maybe a) where {
  showsl = showsl_option;
  showsl_list = showsl_list_option;
};

showsl_prod :: forall a b. (Showl a, Showl b) => (a, b) -> String -> String;
showsl_prod (x, y) =
  (((showsl_lit "(" . showsl x) . showsl_lit ", ") . showsl y) . showsl_lit ")";

showsl_list_prod ::
  forall a b. (Showl a, Showl b) => [(a, b)] -> String -> String;
showsl_list_prod xs = default_showsl_list showsl_prod xs;

instance (Showl a, Showl b) => Showl (a, b) where {
  showsl = showsl_prod;
  showsl_list = showsl_list_prod;
};

add_index :: (String -> String) -> Arith.Nat -> String -> String;
add_index s i = (s . showsl_lit ".") . showsl_nat i;

showsl_sum ::
  forall a b. (Showl a, Showl b) => Sum_Type.Sum a b -> String -> String;
showsl_sum (Sum_Type.Inl x) = (showsl_lit "Inl (" . showsl x) . showsl_lit ")";
showsl_sum (Sum_Type.Inr x) = (showsl_lit "Inr (" . showsl x) . showsl_lit ")";

showsl_literal :: String -> String -> String;
showsl_literal s = showsl_lit s;

}
