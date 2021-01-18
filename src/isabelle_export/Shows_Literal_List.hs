{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Shows_Literal_List(showsl_list) where {

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
import qualified Shows_Literal;

showsl_list :: forall a. (Shows_Literal.Showl a) => [a] -> String -> String;
showsl_list xs = Shows_Literal.showsl_list xs;

showsl_list_list ::
  forall a. (Shows_Literal.Showl a) => [[a]] -> String -> String;
showsl_list_list xs = Shows_Literal.default_showsl_list showsl_list xs;

instance (Shows_Literal.Showl a) => Shows_Literal.Showl [a] where {
  showsl = showsl_list;
  showsl_list = showsl_list_list;
};

}
