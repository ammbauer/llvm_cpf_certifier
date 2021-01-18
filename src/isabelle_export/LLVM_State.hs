{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  LLVM_State(Stack_value(..), Frame(..), Stuck(..), Llvm_state(..),
              mem_new_address, static_error, malloc, option_to_sum, nth_option,
              mem_load, empty_mem, mem, pos, mem_store, update_mem, update_pos,
              stack, update_stack, update_frames, frames, intValue)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Data_Bits;
import qualified HOL;
import qualified Error_Monad;
import qualified Option;
import qualified Impl;
import qualified Sum_Type;
import qualified LLVM_Syntax;
import qualified Mapping;
import qualified Arith;

data Stack_value = IntegerValue Arith.Nat Arith.Int
  | Pointer LLVM_Syntax.Llvm_type Arith.Nat Arith.Nat;

data Frame =
  Frame (LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat))
    (Mapping.Mapping LLVM_Syntax.Name Stack_value) [[Maybe Stack_value]];

data Stuck = StaticError String | ExternalFunctionCall LLVM_Syntax.Name
  | ReturnValue Stack_value | Program_Termination;

data Llvm_state = Llvm_state [Frame] [[Maybe Stack_value]];

mem_new_address :: [[Maybe Stack_value]] -> Arith.Nat;
mem_new_address m = Impl.size_list m;

static_error :: forall a. String -> Sum_Type.Sum Stuck a;
static_error m = Sum_Type.Inl (StaticError m);

malloc ::
  [[Maybe Stack_value]] ->
    LLVM_Syntax.Llvm_type ->
      Arith.Int -> Sum_Type.Sum Stuck (Arith.Nat, [[Maybe Stack_value]]);
malloc m t n =
  (if Arith.less_eq_int Arith.Zero_int n
    then Sum_Type.Inr
           (mem_new_address m, m ++ [Impl.replicate (Arith.nat n) Nothing])
    else static_error "adsf");

option_to_sum :: forall a b. Maybe a -> b -> Sum_Type.Sum b a;
option_to_sum (Just x) uu = Sum_Type.Inr x;
option_to_sum Nothing y = Sum_Type.Inl y;

nth_option :: forall a. [a] -> Arith.Nat -> Maybe a;
nth_option (uu : xs) (Arith.Suc n) = nth_option xs n;
nth_option (x : uv) Arith.Zero_nat = Just x;
nth_option [] uw = Nothing;

mem_load ::
  [[Maybe Stack_value]] ->
    Arith.Nat -> Arith.Nat -> Sum_Type.Sum Stuck Stack_value;
mem_load m a offs =
  Error_Monad.bind
    (option_to_sum (nth_option m a)
      (StaticError "fun load: unallocated address"))
    (\ xs ->
      Error_Monad.bind
        (option_to_sum (Option.bind (nth_option xs offs) id)
          (StaticError "fun load: NULL"))
        Sum_Type.Inr);

empty_mem :: [[Maybe Stack_value]];
empty_mem = [];

mem :: Frame -> [[Maybe Stack_value]];
mem (Frame x1 x2 x3) = x3;

pos :: Frame -> (LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat));
pos (Frame x1 x2 x3) = x1;

mem_store ::
  [[Maybe Stack_value]] ->
    Arith.Nat ->
      Arith.Nat -> Stack_value -> Sum_Type.Sum Stuck [[Maybe Stack_value]];
mem_store m a offs v =
  Error_Monad.bind
    (option_to_sum (nth_option m a)
      (StaticError "fun store: unallocated address"))
    (\ xs -> let {
               xsa = Impl.list_update xs offs (Just v);
             } in Sum_Type.Inr (Impl.list_update m a xsa));

update_mem ::
  ([[Maybe Stack_value]] -> [[Maybe Stack_value]]) -> Frame -> Frame;
update_mem f (Frame p s m) = Frame p s (f m);

update_pos ::
  ((LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat)) ->
    (LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat))) ->
    Frame -> Frame;
update_pos f (Frame p s m) = Frame (f p) s m;

stack :: Frame -> Mapping.Mapping LLVM_Syntax.Name Stack_value;
stack (Frame x1 x2 x3) = x2;

update_stack ::
  (Mapping.Mapping LLVM_Syntax.Name Stack_value ->
    Mapping.Mapping LLVM_Syntax.Name Stack_value) ->
    Frame -> Frame;
update_stack f (Frame p s m) = Frame p (f s) m;

update_frames :: ([Frame] -> [Frame]) -> Llvm_state -> Llvm_state;
update_frames f (Llvm_state fs m) = Llvm_state (f fs) m;

frames :: Llvm_state -> [Frame];
frames (Llvm_state x1 x2) = x1;

intValue :: Stack_value -> Arith.Int;
intValue (IntegerValue x11 x12) = x12;

}
