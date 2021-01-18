{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Error_Monad(bind, isOK, catch_error, choice, forallM, sequence, forallM_index)
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
import qualified Arith;
import qualified Sum_Type;

bind ::
  forall a b c. Sum_Type.Sum a b -> (b -> Sum_Type.Sum a c) -> Sum_Type.Sum a c;
bind m f = (case m of {
             Sum_Type.Inl a -> Sum_Type.Inl a;
             Sum_Type.Inr a -> f a;
           });

isOK :: forall a b. Sum_Type.Sum a b -> Bool;
isOK m = (case m of {
           Sum_Type.Inl _ -> False;
           Sum_Type.Inr _ -> True;
         });

catch_error ::
  forall a b c. Sum_Type.Sum a b -> (a -> Sum_Type.Sum c b) -> Sum_Type.Sum c b;
catch_error m f = (case m of {
                    Sum_Type.Inl a -> f a;
                    Sum_Type.Inr a -> Sum_Type.Inr a;
                  });

choice :: forall a b. [Sum_Type.Sum a b] -> Sum_Type.Sum [a] b;
choice [] = Sum_Type.Inl [];
choice (x : xs) =
  catch_error x
    (\ e -> catch_error (choice xs) (\ xa -> Sum_Type.Inl (e : xa)));

forallM ::
  forall a b. (a -> Sum_Type.Sum b ()) -> [a] -> Sum_Type.Sum (a, b) ();
forallM f [] = Sum_Type.Inr ();
forallM f (x : xs) =
  bind (catch_error (f x) (\ xa -> Sum_Type.Inl (x, xa))) (\ _ -> forallM f xs);

sequence :: forall a b. [Sum_Type.Sum a b] -> Sum_Type.Sum a [b];
sequence [] = Sum_Type.Inr [];
sequence (m : ms) =
  bind m (\ x -> bind (sequence ms) (\ xs -> Sum_Type.Inr (x : xs)));

forallM_index_aux ::
  forall a b.
    (a -> Arith.Nat -> Sum_Type.Sum b ()) ->
      Arith.Nat -> [a] -> Sum_Type.Sum ((a, Arith.Nat), b) ();
forallM_index_aux p i [] = Sum_Type.Inr ();
forallM_index_aux p i (x : xs) =
  bind (catch_error (p x i) (\ xa -> Sum_Type.Inl ((x, i), xa)))
    (\ _ -> forallM_index_aux p (Arith.suc i) xs);

forallM_index ::
  forall a b.
    (a -> Arith.Nat -> Sum_Type.Sum b ()) ->
      [a] -> Sum_Type.Sum ((a, Arith.Nat), b) ();
forallM_index p xs = forallM_index_aux p Arith.zero_nat xs;

}
