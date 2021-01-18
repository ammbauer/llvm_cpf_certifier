{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  Impl(Ceq(..), Set_impla(..), Set_impl(..), Cenum(..), Finite_UNIV(..),
        Ccompare(..), Color(..), Rbt(..), Mapping_rbt(..), Set_dlist(..),
        Set(..), foldl, foldr, balance, rbt_comp_ins, paint,
        rbt_comp_insert_with_key, rbt_comp_insert, impl_of, insertb,
        list_of_dlist, list_member, list_insert, inserta, balance_right,
        balance_left, combine, rbt_comp_del, rbt_comp_del_from_left,
        rbt_comp_del_from_right, rbt_comp_delete, delete, list_remove1, removea,
        insert, remove, memberb, rbt_comp_lookup, lookup, membera, member,
        collect, emptya, empty, set_empty_choose, set_empty, set_aux, set,
        remdups, mapa, gen_keys, keys, keysa, replicate, gen_length,
        list_update, mapb, finite, insort_key, sort_key, size_list,
        sorted_list_of_set)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Data_Bits;
import qualified Orders;
import qualified Arith;
import qualified Product_Type;
import qualified Closure_Set;
import qualified Option;
import qualified Comparator;
import qualified Phantom_Type;

class Ceq a where {
  ceq :: Maybe (a -> a -> Bool);
};

data Set_impla = Set_Choose | Set_Collect | Set_DList | Set_RBT | Set_Monada;

class Set_impl a where {
  set_impl :: Phantom_Type.Phantom a Set_impla;
};

class Cenum a where {
  cEnum :: Maybe ([a], ((a -> Bool) -> Bool, (a -> Bool) -> Bool));
};

class Finite_UNIV a where {
  finite_UNIV :: Phantom_Type.Phantom a Bool;
};

class Ccompare a where {
  ccompare :: Maybe (a -> a -> Comparator.Order);
};

data Color = R | B;

data Rbt a b = Empty | Branch Color (Rbt a b) a b (Rbt a b);

newtype Mapping_rbt b a = Mapping_RBT (Rbt b a);

newtype Set_dlist a = Abs_dlist [a];

data Set a = Collect_set (a -> Bool) | DList_set (Set_dlist a)
  | RBT_set (Mapping_rbt a ()) | Set_Monad [a] | Complement (Set a);

foldl :: forall a b. (a -> b -> a) -> a -> [b] -> a;
foldl f a [] = a;
foldl f a (x : xs) = foldl f (f a x) xs;

foldr :: forall a b. (a -> b -> b) -> [a] -> b -> b;
foldr f [] = id;
foldr f (x : xs) = f x . foldr f xs;

balance :: forall a b. Rbt a b -> a -> b -> Rbt a b -> Rbt a b;
balance (Branch R a w x b) s t (Branch R c y z d) =
  Branch R (Branch B a w x b) s t (Branch B c y z d);
balance (Branch R (Branch R a w x b) s t c) y z Empty =
  Branch R (Branch B a w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch R a w x b) s t c) y z (Branch B va vb vc vd) =
  Branch R (Branch B a w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R Empty w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance (Branch R (Branch B va vb vc vd) w x (Branch R b s t c)) y z Empty =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch R Empty w x (Branch R b s t c)) y z (Branch B va vb vc vd) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch R (Branch B ve vf vg vh) w x (Branch R b s t c)) y z
  (Branch B va vb vc vd) =
  Branch R (Branch B (Branch B ve vf vg vh) w x b) s t
    (Branch B c y z (Branch B va vb vc vd));
balance Empty w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z d);
balance (Branch B va vb vc vd) w x (Branch R b s t (Branch R c y z d)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z d);
balance Empty w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z Empty);
balance Empty w x (Branch R (Branch R b s t c) y z (Branch B va vb vc vd)) =
  Branch R (Branch B Empty w x b) s t (Branch B c y z (Branch B va vb vc vd));
balance (Branch B va vb vc vd) w x (Branch R (Branch R b s t c) y z Empty) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t (Branch B c y z Empty);
balance (Branch B va vb vc vd) w x
  (Branch R (Branch R b s t c) y z (Branch B ve vf vg vh)) =
  Branch R (Branch B (Branch B va vb vc vd) w x b) s t
    (Branch B c y z (Branch B ve vf vg vh));
balance Empty s t Empty = Branch B Empty s t Empty;
balance Empty s t (Branch B va vb vc vd) =
  Branch B Empty s t (Branch B va vb vc vd);
balance Empty s t (Branch v Empty vb vc Empty) =
  Branch B Empty s t (Branch v Empty vb vc Empty);
balance Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty) =
  Branch B Empty s t (Branch v (Branch B ve vf vg vh) vb vc Empty);
balance Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi)) =
  Branch B Empty s t (Branch v Empty vb vc (Branch B vf vg vh vi));
balance Empty s t (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi))
  = Branch B Empty s t
      (Branch v (Branch B ve vj vk vl) vb vc (Branch B vf vg vh vi));
balance (Branch B va vb vc vd) s t Empty =
  Branch B (Branch B va vb vc vd) s t Empty;
balance (Branch B va vb vc vd) s t (Branch B ve vf vg vh) =
  Branch B (Branch B va vb vc vd) s t (Branch B ve vf vg vh);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty) =
  Branch B (Branch B va vb vc vd) s t (Branch v Empty vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v (Branch B vi vj vk vl) vf vg Empty)
  = Branch B (Branch B va vb vc vd) s t
      (Branch v (Branch B vi vj vk vl) vf vg Empty);
balance (Branch B va vb vc vd) s t (Branch v Empty vf vg (Branch B vj vk vl vm))
  = Branch B (Branch B va vb vc vd) s t
      (Branch v Empty vf vg (Branch B vj vk vl vm));
balance (Branch B va vb vc vd) s t
  (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm)) =
  Branch B (Branch B va vb vc vd) s t
    (Branch v (Branch B vi vn vo vp) vf vg (Branch B vj vk vl vm));
balance (Branch v Empty vb vc Empty) s t Empty =
  Branch B (Branch v Empty vb vc Empty) s t Empty;
balance (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty =
  Branch B (Branch v Empty vb vc (Branch B ve vf vg vh)) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty =
  Branch B (Branch v (Branch B vf vg vh vi) vb vc Empty) s t Empty;
balance (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t Empty
  = Branch B (Branch v (Branch B vf vg vh vi) vb vc (Branch B ve vj vk vl)) s t
      Empty;
balance (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd) =
  Branch B (Branch v Empty vf vg Empty) s t (Branch B va vb vc vd);
balance (Branch v Empty vf vg (Branch B vi vj vk vl)) s t (Branch B va vb vc vd)
  = Branch B (Branch v Empty vf vg (Branch B vi vj vk vl)) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg Empty) s t (Branch B va vb vc vd)
  = Branch B (Branch v (Branch B vj vk vl vm) vf vg Empty) s t
      (Branch B va vb vc vd);
balance (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
  (Branch B va vb vc vd) =
  Branch B (Branch v (Branch B vj vk vl vm) vf vg (Branch B vi vn vo vp)) s t
    (Branch B va vb vc vd);

rbt_comp_ins ::
  forall a b.
    (a -> a -> Comparator.Order) ->
      (a -> b -> b -> b) -> a -> b -> Rbt a b -> Rbt a b;
rbt_comp_ins c f k v Empty = Branch R Empty k v Empty;
rbt_comp_ins c f k v (Branch B l x y r) =
  (case c k x of {
    Comparator.Eqa -> Branch B l x (f k y v) r;
    Comparator.Lt -> balance (rbt_comp_ins c f k v l) x y r;
    Comparator.Gt -> balance l x y (rbt_comp_ins c f k v r);
  });
rbt_comp_ins c f k v (Branch R l x y r) =
  (case c k x of {
    Comparator.Eqa -> Branch R l x (f k y v) r;
    Comparator.Lt -> Branch R (rbt_comp_ins c f k v l) x y r;
    Comparator.Gt -> Branch R l x y (rbt_comp_ins c f k v r);
  });

paint :: forall a b. Color -> Rbt a b -> Rbt a b;
paint c Empty = Empty;
paint c (Branch uu l k v r) = Branch c l k v r;

rbt_comp_insert_with_key ::
  forall a b.
    (a -> a -> Comparator.Order) ->
      (a -> b -> b -> b) -> a -> b -> Rbt a b -> Rbt a b;
rbt_comp_insert_with_key c f k v t = paint B (rbt_comp_ins c f k v t);

rbt_comp_insert ::
  forall a b. (a -> a -> Comparator.Order) -> a -> b -> Rbt a b -> Rbt a b;
rbt_comp_insert c = rbt_comp_insert_with_key c (\ _ _ nv -> nv);

impl_of :: forall b a. (Ccompare b) => Mapping_rbt b a -> Rbt b a;
impl_of (Mapping_RBT x) = x;

insertb ::
  forall a b. (Ccompare a) => a -> b -> Mapping_rbt a b -> Mapping_rbt a b;
insertb xc xd xe =
  Mapping_RBT (rbt_comp_insert (Option.the ccompare) xc xd (impl_of xe));

list_of_dlist :: forall a. (Ceq a) => Set_dlist a -> [a];
list_of_dlist (Abs_dlist x) = x;

list_member :: forall a. (a -> a -> Bool) -> [a] -> a -> Bool;
list_member equal (x : xs) y = equal x y || list_member equal xs y;
list_member equal [] y = False;

list_insert :: forall a. (a -> a -> Bool) -> a -> [a] -> [a];
list_insert equal x xs = (if list_member equal xs x then xs else x : xs);

inserta :: forall a. (Ceq a) => a -> Set_dlist a -> Set_dlist a;
inserta xb xc = Abs_dlist (list_insert (Option.the ceq) xb (list_of_dlist xc));

balance_right :: forall a b. Rbt a b -> a -> b -> Rbt a b -> Rbt a b;
balance_right a k x (Branch R b s y c) = Branch R a k x (Branch B b s y c);
balance_right (Branch B a k x b) s y Empty =
  balance (Branch R a k x b) s y Empty;
balance_right (Branch B a k x b) s y (Branch B va vb vc vd) =
  balance (Branch R a k x b) s y (Branch B va vb vc vd);
balance_right (Branch R a k x (Branch B b s y c)) t z Empty =
  Branch R (balance (paint R a) k x b) s y (Branch B c t z Empty);
balance_right (Branch R a k x (Branch B b s y c)) t z (Branch B va vb vc vd) =
  Branch R (balance (paint R a) k x b) s y
    (Branch B c t z (Branch B va vb vc vd));
balance_right Empty k x Empty = Empty;
balance_right (Branch R va vb vc Empty) k x Empty = Empty;
balance_right (Branch R va vb vc (Branch R ve vf vg vh)) k x Empty = Empty;
balance_right Empty k x (Branch B va vb vc vd) = Empty;
balance_right (Branch R ve vf vg Empty) k x (Branch B va vb vc vd) = Empty;
balance_right (Branch R ve vf vg (Branch R vi vj vk vl)) k x
  (Branch B va vb vc vd) = Empty;

balance_left :: forall a b. Rbt a b -> a -> b -> Rbt a b -> Rbt a b;
balance_left (Branch R a k x b) s y c = Branch R (Branch B a k x b) s y c;
balance_left Empty k x (Branch B a s y b) =
  balance Empty k x (Branch R a s y b);
balance_left (Branch B va vb vc vd) k x (Branch B a s y b) =
  balance (Branch B va vb vc vd) k x (Branch R a s y b);
balance_left Empty k x (Branch R (Branch B a s y b) t z c) =
  Branch R (Branch B Empty k x a) s y (balance b t z (paint R c));
balance_left (Branch B va vb vc vd) k x (Branch R (Branch B a s y b) t z c) =
  Branch R (Branch B (Branch B va vb vc vd) k x a) s y
    (balance b t z (paint R c));
balance_left Empty k x Empty = Empty;
balance_left Empty k x (Branch R Empty vb vc vd) = Empty;
balance_left Empty k x (Branch R (Branch R ve vf vg vh) vb vc vd) = Empty;
balance_left (Branch B va vb vc vd) k x Empty = Empty;
balance_left (Branch B va vb vc vd) k x (Branch R Empty vf vg vh) = Empty;
balance_left (Branch B va vb vc vd) k x
  (Branch R (Branch R vi vj vk vl) vf vg vh) = Empty;

combine :: forall a b. Rbt a b -> Rbt a b -> Rbt a b;
combine Empty x = x;
combine (Branch v va vb vc vd) Empty = Branch v va vb vc vd;
combine (Branch R a k x b) (Branch R c s y d) =
  (case combine b c of {
    Empty -> Branch R a k x (Branch R Empty s y d);
    Branch R b2 t z c2 -> Branch R (Branch R a k x b2) t z (Branch R c2 s y d);
    Branch B b2 t z c2 -> Branch R a k x (Branch R (Branch B b2 t z c2) s y d);
  });
combine (Branch B a k x b) (Branch B c s y d) =
  (case combine b c of {
    Empty -> balance_left a k x (Branch B Empty s y d);
    Branch R b2 t z c2 -> Branch R (Branch B a k x b2) t z (Branch B c2 s y d);
    Branch B b2 t z c2 ->
      balance_left a k x (Branch B (Branch B b2 t z c2) s y d);
  });
combine (Branch B va vb vc vd) (Branch R b k x c) =
  Branch R (combine (Branch B va vb vc vd) b) k x c;
combine (Branch R a k x b) (Branch B va vb vc vd) =
  Branch R a k x (combine b (Branch B va vb vc vd));

rbt_comp_del ::
  forall a b. (a -> a -> Comparator.Order) -> a -> Rbt a b -> Rbt a b;
rbt_comp_del c x Empty = Empty;
rbt_comp_del c x (Branch uu a y s b) =
  (case c x y of {
    Comparator.Eqa -> combine a b;
    Comparator.Lt -> rbt_comp_del_from_left c x a y s b;
    Comparator.Gt -> rbt_comp_del_from_right c x a y s b;
  });

rbt_comp_del_from_left ::
  forall a b.
    (a -> a -> Comparator.Order) ->
      a -> Rbt a b -> a -> b -> Rbt a b -> Rbt a b;
rbt_comp_del_from_left c x (Branch B lt z v rt) y s b =
  balance_left (rbt_comp_del c x (Branch B lt z v rt)) y s b;
rbt_comp_del_from_left c x Empty y s b =
  Branch R (rbt_comp_del c x Empty) y s b;
rbt_comp_del_from_left c x (Branch R va vb vc vd) y s b =
  Branch R (rbt_comp_del c x (Branch R va vb vc vd)) y s b;

rbt_comp_del_from_right ::
  forall a b.
    (a -> a -> Comparator.Order) ->
      a -> Rbt a b -> a -> b -> Rbt a b -> Rbt a b;
rbt_comp_del_from_right c x a y s (Branch B lt z v rt) =
  balance_right a y s (rbt_comp_del c x (Branch B lt z v rt));
rbt_comp_del_from_right c x a y s Empty =
  Branch R a y s (rbt_comp_del c x Empty);
rbt_comp_del_from_right c x a y s (Branch R va vb vc vd) =
  Branch R a y s (rbt_comp_del c x (Branch R va vb vc vd));

rbt_comp_delete ::
  forall a b. (a -> a -> Comparator.Order) -> a -> Rbt a b -> Rbt a b;
rbt_comp_delete c k t = paint B (rbt_comp_del c k t);

delete :: forall a b. (Ccompare a) => a -> Mapping_rbt a b -> Mapping_rbt a b;
delete xb xc =
  Mapping_RBT (rbt_comp_delete (Option.the ccompare) xb (impl_of xc));

list_remove1 :: forall a. (a -> a -> Bool) -> a -> [a] -> [a];
list_remove1 equal x (y : xs) =
  (if equal x y then xs else y : list_remove1 equal x xs);
list_remove1 equal x [] = [];

removea :: forall a. (Ceq a) => a -> Set_dlist a -> Set_dlist a;
removea xb xc = Abs_dlist (list_remove1 (Option.the ceq) xb (list_of_dlist xc));

insert :: forall a. (Ceq a, Ccompare a) => a -> Set a -> Set a;
insert xa (Complement x) = Complement (remove xa x);
insert x (RBT_set rbt) =
  (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "insert RBT_set: ccompare = None" (\ _ -> insert x (RBT_set rbt));
    Just _ -> RBT_set (insertb x () rbt);
  });
insert x (DList_set dxs) =
  (case (ceq :: Maybe (a -> a -> Bool)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "insert DList_set: ceq = None" (\ _ -> insert x (DList_set dxs));
    Just _ -> DList_set (inserta x dxs);
  });
insert x (Set_Monad xs) = Set_Monad (x : xs);
insert x (Collect_set a) =
  (case ceq of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "insert Collect_set: ceq = None" (\ _ -> insert x (Collect_set a));
    Just eq -> Collect_set (Closure_Set.fun_upd eq a x True);
  });

remove :: forall a. (Ceq a, Ccompare a) => a -> Set a -> Set a;
remove x (Complement a) = Complement (insert x a);
remove x (RBT_set rbt) =
  (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "remove RBT_set: ccompare = None" (\ _ -> remove x (RBT_set rbt));
    Just _ -> RBT_set (delete x rbt);
  });
remove x (DList_set dxs) =
  (case (ceq :: Maybe (a -> a -> Bool)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "remove DList_set: ceq = None" (\ _ -> remove x (DList_set dxs));
    Just _ -> DList_set (removea x dxs);
  });
remove x (Collect_set a) =
  (case ceq of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a) "remove Collect: ceq = None"
        (\ _ -> remove x (Collect_set a));
    Just eq -> Collect_set (Closure_Set.fun_upd eq a x False);
  });

memberb :: forall a. (Ceq a) => Set_dlist a -> a -> Bool;
memberb xa = list_member (Option.the ceq) (list_of_dlist xa);

rbt_comp_lookup ::
  forall a b. (a -> a -> Comparator.Order) -> Rbt a b -> a -> Maybe b;
rbt_comp_lookup c Empty k = Nothing;
rbt_comp_lookup c (Branch uu l x y r) k =
  (case c k x of {
    Comparator.Eqa -> Just y;
    Comparator.Lt -> rbt_comp_lookup c l k;
    Comparator.Gt -> rbt_comp_lookup c r k;
  });

lookup :: forall a b. (Ccompare a) => Mapping_rbt a b -> a -> Maybe b;
lookup xa = rbt_comp_lookup (Option.the ccompare) (impl_of xa);

membera :: forall a. (Ccompare a) => Mapping_rbt a () -> a -> Bool;
membera t x = lookup t x == Just ();

member :: forall a. (Ceq a, Ccompare a) => a -> Set a -> Bool;
member x (Set_Monad xs) =
  (case ceq of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "member Set_Monad: ceq = None" (\ _ -> member x (Set_Monad xs));
    Just eq -> list_member eq xs x;
  });
member xa (Complement x) = not (member xa x);
member x (RBT_set rbt) = membera rbt x;
member x (DList_set dxs) = memberb dxs x;
member x (Collect_set a) = a x;

collect :: forall a. (Cenum a) => (a -> Bool) -> Set a;
collect p = (case cEnum of {
              Nothing -> Collect_set p;
              Just (enum, _) -> Set_Monad (filter p enum);
            });

emptya :: forall a b. (Ccompare a) => Mapping_rbt a b;
emptya = Mapping_RBT Empty;

empty :: forall a. (Ceq a) => Set_dlist a;
empty = Abs_dlist [];

set_empty_choose :: forall a. (Ceq a, Ccompare a) => Set a;
set_empty_choose = (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
                     Nothing -> (case (ceq :: Maybe (a -> a -> Bool)) of {
                                  Nothing -> Set_Monad [];
                                  Just _ -> DList_set empty;
                                });
                     Just _ -> RBT_set emptya;
                   });

set_empty :: forall a. (Ceq a, Ccompare a) => Set_impla -> Set a;
set_empty Set_Choose = set_empty_choose;
set_empty Set_Monada = Set_Monad [];
set_empty Set_RBT = RBT_set emptya;
set_empty Set_DList = DList_set empty;
set_empty Set_Collect = Collect_set (\ _ -> False);

set_aux :: forall a. (Ceq a, Ccompare a) => Set_impla -> [a] -> Set a;
set_aux Set_Monada = Set_Monad;
set_aux Set_Choose =
  (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing -> (case (ceq :: Maybe (a -> a -> Bool)) of {
                 Nothing -> Set_Monad;
                 Just _ -> foldl (\ s x -> insert x s) (DList_set empty);
               });
    Just _ -> foldl (\ s x -> insert x s) (RBT_set emptya);
  });
set_aux impl = foldl (\ s x -> insert x s) (set_empty impl);

set :: forall a. (Ceq a, Ccompare a, Set_impl a) => [a] -> Set a;
set xs =
  set_aux
    (Phantom_Type.of_phantom (set_impl :: Phantom_Type.Phantom a Set_impla)) xs;

remdups :: forall a. (Ceq a, Ccompare a, Set_impl a) => [a] -> [a];
remdups [] = [];
remdups (x : xs) = (if member x (set xs) then remdups xs else x : remdups xs);

mapa :: forall a b c. (a -> b -> c) -> Rbt a b -> Rbt a c;
mapa f Empty = Empty;
mapa f (Branch c lt k v rt) = Branch c (mapa f lt) k (f k v) (mapa f rt);

gen_keys :: forall a b. [(a, Rbt a b)] -> Rbt a b -> [a];
gen_keys kts (Branch c l k v r) = gen_keys ((k, r) : kts) l;
gen_keys ((k, t) : kts) Empty = k : gen_keys kts t;
gen_keys [] Empty = [];

keys :: forall a b. Rbt a b -> [a];
keys = gen_keys [];

keysa :: forall a. (Ccompare a) => Mapping_rbt a () -> [a];
keysa xa = keys (impl_of xa);

replicate :: forall a. Arith.Nat -> a -> [a];
replicate Arith.Zero_nat x = [];
replicate (Arith.Suc n) x = x : replicate n x;

gen_length :: forall a. Arith.Nat -> [a] -> Arith.Nat;
gen_length n (x : xs) = gen_length (Arith.Suc n) xs;
gen_length n [] = n;

list_update :: forall a. [a] -> Arith.Nat -> a -> [a];
list_update (x : xs) (Arith.Suc i) y = x : list_update xs i y;
list_update (x : xs) Arith.Zero_nat y = y : xs;
list_update [] i y = [];

mapb ::
  forall a c b.
    (Ccompare a) => (a -> c -> b) -> Mapping_rbt a c -> Mapping_rbt a b;
mapb xb xc = Mapping_RBT (mapa xb (impl_of xc));

finite :: forall a. (Finite_UNIV a, Ceq a, Ccompare a) => Set a -> Bool;
finite (Collect_set p) =
  Phantom_Type.of_phantom (finite_UNIV :: Phantom_Type.Phantom a Bool) ||
    (error :: forall a. String -> (() -> a) -> a) "finite Collect_set"
      (\ _ -> finite (Collect_set p));
finite (Set_Monad xs) = True;
finite (Complement a) =
  (if Phantom_Type.of_phantom (finite_UNIV :: Phantom_Type.Phantom a Bool)
    then True
    else (if finite a then False
           else (error :: forall a. String -> (() -> a) -> a)
                  "finite Complement: infinite set"
                  (\ _ -> finite (Complement a))));
finite (RBT_set rbt) =
  (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "finite RBT_set: ccompare = None" (\ _ -> finite (RBT_set rbt));
    Just _ -> True;
  });
finite (DList_set dxs) =
  (case (ceq :: Maybe (a -> a -> Bool)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "finite DList_set: ceq = None" (\ _ -> finite (DList_set dxs));
    Just _ -> True;
  });

insort_key :: forall a b. (Orders.Linorder b) => (a -> b) -> a -> [a] -> [a];
insort_key f x [] = [x];
insort_key f x (y : ys) =
  (if Orders.less_eq (f x) (f y) then x : y : ys else y : insort_key f x ys);

sort_key :: forall a b. (Orders.Linorder b) => (a -> b) -> [a] -> [a];
sort_key f xs = foldr (insort_key f) xs [];

size_list :: forall a. [a] -> Arith.Nat;
size_list = gen_length Arith.Zero_nat;

sorted_list_of_set ::
  forall a. (Ceq a, Ccompare a, Orders.Linorder a, Set_impl a) => Set a -> [a];
sorted_list_of_set (RBT_set rbt) =
  (case (ccompare :: Maybe (a -> a -> Comparator.Order)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "sorted_list_of_set RBT_set: ccompare = None"
        (\ _ -> sorted_list_of_set (RBT_set rbt));
    Just _ -> sort_key (\ x -> x) (keysa rbt);
  });
sorted_list_of_set (DList_set dxs) =
  (case (ceq :: Maybe (a -> a -> Bool)) of {
    Nothing ->
      (error :: forall a. String -> (() -> a) -> a)
        "sorted_list_of_set DList_set: ceq = None"
        (\ _ -> sorted_list_of_set (DList_set dxs));
    Just _ -> sort_key (\ x -> x) (list_of_dlist dxs);
  });
sorted_list_of_set (Set_Monad xs) = sort_key (\ x -> x) (remdups xs);

}
