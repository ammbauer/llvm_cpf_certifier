{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  LLVM_Run_Prog_HS(showsl_mapping, showsl_stack_value, showsl_list_stack_value,
                    ccompare_name, cEnum_name, set_impl_name, ceq_name,
                    showsl_frame, showsl_list_frame, showsl_stuck,
                    showsl_list_stuck, showsl_llvm_state,
                    showsl_list_llvm_state, Action(..), option_to_sum,
                    option_to_error, operand_value, phi_bid, compute_phi,
                    compute_phis, c_pos, blocks, find_block, find_fun,
                    find_fun_block, find_phis, update_bid_frame, static_error,
                    condBr_to_frame, update_stack, inc_pos, ret_from_frame,
                    update_frame, find_action, find_statement, terminate_frame,
                    update_frames_stack, binop_instruction,
                    select_llvm_constant, operand_select, operand_binop,
                    map_of_funs, mapping_impl_name, zip_parameters, enter_frame,
                    call_function, allocate, binop_llvm_constant,
                    icmp_llvm_constant, store, load, run_instruction, step,
                    step_by_step, run_prog, run_prog_string)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified Data_Bits;
import qualified Orders;
import qualified Shows_Literal_List;
import qualified Bits_Int;
import qualified LLVM_Parser;
import qualified Product_Type;
import qualified Error_Monad;
import qualified Option;
import qualified Mapping;
import qualified Map;
import qualified HOL;
import qualified Sum_Type;
import qualified Arith;
import qualified Comparator;
import qualified Phantom_Type;
import qualified LLVM_Syntax;
import qualified Impl;
import qualified LLVM_State;
import qualified Shows_Literal;

showsl_mapping ::
  forall a b.
    (Impl.Finite_UNIV a, Impl.Cenum a, Impl.Ceq a, Impl.Ccompare a, Eq a,
      Orders.Linorder a, Impl.Set_impl a, Shows_Literal.Showl a,
      Shows_Literal.Showl b) => Mapping.Mapping a b -> String -> String;
showsl_mapping m =
  Shows_Literal_List.showsl_list
    (map (\ x -> (x, Mapping.lookup m x)) (Mapping.ordered_keys m));

showsl_stack_value :: LLVM_State.Stack_value -> String -> String;
showsl_stack_value (LLVM_State.IntegerValue l i) =
  ((Shows_Literal.showsl_literal "i" . Shows_Literal.showsl_nat l) .
    Shows_Literal.showsl_literal " ") .
    Shows_Literal.showsl_int i;
showsl_stack_value (LLVM_State.Pointer t a os) =
  (((LLVM_Syntax.showsl_llvm_type t . Shows_Literal.showsl_literal " ") .
     Shows_Literal.showsl_nat a) .
    Shows_Literal.showsl_literal "+") .
    Shows_Literal.showsl_nat os;

showsl_list_stack_value :: [LLVM_State.Stack_value] -> String -> String;
showsl_list_stack_value xs =
  Shows_Literal.default_showsl_list showsl_stack_value xs;

instance Shows_Literal.Showl LLVM_State.Stack_value where {
  showsl = showsl_stack_value;
  showsl_list = showsl_list_stack_value;
};

ccompare_name ::
  Maybe (LLVM_Syntax.Name -> LLVM_Syntax.Name -> Comparator.Order);
ccompare_name = Just Comparator.comparator_of;

instance Impl.Ccompare LLVM_Syntax.Name where {
  ccompare = ccompare_name;
};

cEnum_name ::
  Maybe ([LLVM_Syntax.Name],
          ((LLVM_Syntax.Name -> Bool) -> Bool,
            (LLVM_Syntax.Name -> Bool) -> Bool));
cEnum_name = Nothing;

instance Impl.Cenum LLVM_Syntax.Name where {
  cEnum = cEnum_name;
};

set_impl_name :: Phantom_Type.Phantom LLVM_Syntax.Name Impl.Set_impla;
set_impl_name = Phantom_Type.Phantom Impl.Set_RBT;

instance Impl.Set_impl LLVM_Syntax.Name where {
  set_impl = set_impl_name;
};

ceq_name :: Maybe (LLVM_Syntax.Name -> LLVM_Syntax.Name -> Bool);
ceq_name = Just LLVM_Syntax.equal_name;

instance Impl.Ceq LLVM_Syntax.Name where {
  ceq = ceq_name;
};

showsl_frame :: LLVM_State.Frame -> String -> String;
showsl_frame (LLVM_State.Frame p s m) =
  ((((Shows_Literal.showsl_literal "Frame:\nPos: " .
       Shows_Literal.showsl_prod p) .
      Shows_Literal.showsl_literal "\nStack") .
     showsl_mapping s) .
    Shows_Literal.showsl_literal "\nHeap:") .
    Shows_Literal_List.showsl_list m;

showsl_list_frame :: [LLVM_State.Frame] -> String -> String;
showsl_list_frame xs = Shows_Literal.default_showsl_list showsl_frame xs;

instance Shows_Literal.Showl LLVM_State.Frame where {
  showsl = showsl_frame;
  showsl_list = showsl_list_frame;
};

showsl_stuck :: LLVM_State.Stuck -> String -> String;
showsl_stuck (LLVM_State.StaticError s) =
  Shows_Literal.showsl_literal "StaticError:  " .
    Shows_Literal.showsl_literal s;
showsl_stuck (LLVM_State.ExternalFunctionCall f) =
  Shows_Literal.showsl_literal "ExternalFunction:  " .
    LLVM_Syntax.showsl_name f;
showsl_stuck (LLVM_State.ReturnValue c) =
  Shows_Literal.showsl_literal "ReturnValue: " . showsl_stack_value c;
showsl_stuck LLVM_State.Program_Termination =
  Shows_Literal.showsl_literal "Program_Termination";

showsl_list_stuck :: [LLVM_State.Stuck] -> String -> String;
showsl_list_stuck xs = Shows_Literal.default_showsl_list showsl_stuck xs;

instance Shows_Literal.Showl LLVM_State.Stuck where {
  showsl = showsl_stuck;
  showsl_list = showsl_list_stuck;
};

showsl_llvm_state :: LLVM_State.Llvm_state -> String -> String;
showsl_llvm_state (LLVM_State.Llvm_state s m) =
  ((Shows_Literal.showsl_literal "LLVM_State: " .
     Shows_Literal_List.showsl_list s) .
    Shows_Literal.showsl_literal "\nGlobal Heap: ") .
    Shows_Literal_List.showsl_list m;

showsl_list_llvm_state :: [LLVM_State.Llvm_state] -> String -> String;
showsl_list_llvm_state xs =
  Shows_Literal.default_showsl_list showsl_llvm_state xs;

instance Shows_Literal.Showl LLVM_State.Llvm_state where {
  showsl = showsl_llvm_state;
  showsl_list = showsl_list_llvm_state;
};

data Action = Instruction LLVM_Syntax.Instruction
  | Terminator LLVM_Syntax.Terminator;

option_to_sum :: forall a b. Maybe a -> b -> Sum_Type.Sum b a;
option_to_sum (Just x) uu = Sum_Type.Inr x;
option_to_sum Nothing y = Sum_Type.Inl y;

option_to_error ::
  forall a. Maybe a -> String -> Sum_Type.Sum LLVM_State.Stuck a;
option_to_error x m = option_to_sum x (LLVM_State.StaticError m);

operand_value ::
  LLVM_State.Frame ->
    LLVM_Syntax.Operand -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value;
operand_value cf (LLVM_Syntax.ConstantOperand i) =
  Sum_Type.Inr
    (LLVM_State.IntegerValue (LLVM_Syntax.integerBits i)
      (LLVM_Syntax.integerValue i));
operand_value cf (LLVM_Syntax.LocalReference n) =
  option_to_error (Mapping.lookup (LLVM_State.stack cf) n)
    "Could not find register";

phi_bid :: forall a b. (Eq a) => a -> [(b, a)] -> Maybe b;
phi_bid old_b_id ps = Map.map_of (map Product_Type.swap ps) old_b_id;

compute_phi ::
  forall a.
    (Eq a) => LLVM_State.Frame ->
                a -> [(LLVM_Syntax.Operand, a)] ->
                       Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value;
compute_phi cf old_b_id xs =
  Error_Monad.bind
    (option_to_error (phi_bid old_b_id xs)
      "Previous block not found in phi expression")
    (operand_value cf);

compute_phis ::
  LLVM_State.Frame ->
    LLVM_Syntax.Name ->
      [(LLVM_Syntax.Name, [(LLVM_Syntax.Operand, LLVM_Syntax.Name)])] ->
        Sum_Type.Sum LLVM_State.Stuck
          [(LLVM_Syntax.Name, LLVM_State.Stack_value)];
compute_phis cf uu [] = Sum_Type.Inr [];
compute_phis cf old_b_id ((a, ps) : asa) =
  Error_Monad.bind (compute_phi cf old_b_id ps)
    (\ c ->
      Error_Monad.bind (compute_phis cf old_b_id asa)
        (\ s -> Sum_Type.Inr ((a, c) : s)));

c_pos :: LLVM_State.Frame -> (LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat));
c_pos cf = LLVM_State.pos cf;

blocks :: LLVM_Syntax.Llvm_fun -> [LLVM_Syntax.Basic_block];
blocks f = LLVM_Syntax.hd_blocks f : LLVM_Syntax.tl_blocks f;

find_block ::
  LLVM_Syntax.Llvm_fun ->
    LLVM_Syntax.Name -> Sum_Type.Sum LLVM_State.Stuck LLVM_Syntax.Basic_block;
find_block f n = let {
                   g = (\ b -> (LLVM_Syntax.name b, b));
                   blocksa = Map.map_of (map g (blocks f));
                 } in option_to_error (blocksa n) "Cannot find block";

find_fun ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_Syntax.Name -> Sum_Type.Sum LLVM_State.Stuck LLVM_Syntax.Llvm_fun;
find_fun prog n = let {
                    g = (\ f -> (LLVM_Syntax.fun_name f, f));
                    proga = Map.map_of (map g (LLVM_Syntax.funs prog));
                  } in option_to_error (proga n) "Cannot find function";

find_fun_block ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_Syntax.Name ->
      LLVM_Syntax.Name -> Sum_Type.Sum LLVM_State.Stuck LLVM_Syntax.Basic_block;
find_fun_block prog fn bn =
  Error_Monad.bind (find_fun prog fn) (\ f -> find_block f bn);

find_phis ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_Syntax.Name ->
      LLVM_Syntax.Name ->
        Sum_Type.Sum LLVM_State.Stuck
          [(LLVM_Syntax.Name, [(LLVM_Syntax.Operand, LLVM_Syntax.Name)])];
find_phis prog fn bn =
  Sum_Type.map_sum id LLVM_Syntax.phis (find_fun_block prog fn bn);

update_bid_frame ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Frame ->
      LLVM_Syntax.Name -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Frame;
update_bid_frame lc cf new_b_id =
  (case c_pos cf of {
    (func_id, (old_b_id, _)) ->
      Error_Monad.bind (find_phis lc func_id new_b_id)
        (\ phi_s ->
          Error_Monad.bind (compute_phis cf old_b_id phi_s)
            (\ s ->
              let {
                sa = Impl.foldr (\ (a, b) -> Mapping.update a b) s
                       (LLVM_State.stack cf);
              } in Sum_Type.Inr
                     (LLVM_State.update_pos
                       (\ _ -> (func_id, (new_b_id, Impl.size_list phi_s)))
                       (LLVM_State.update_stack (\ _ -> sa) cf))));
  });

static_error :: forall a. String -> Sum_Type.Sum LLVM_State.Stuck a;
static_error m = Sum_Type.Inl (LLVM_State.StaticError m);

condBr_to_frame ::
  forall a. LLVM_State.Stack_value -> a -> a -> Sum_Type.Sum LLVM_State.Stuck a;
condBr_to_frame (LLVM_State.Pointer v va vb) uv uw = static_error "ill-typed";
condBr_to_frame (LLVM_State.IntegerValue l i) id_t id_f =
  (if Arith.equal_nat l Arith.one_nat && Arith.equal_int i Arith.one_int
    then Sum_Type.Inr id_t
    else (if Arith.equal_nat l Arith.one_nat && Arith.equal_int i Arith.Zero_int
           then Sum_Type.Inr id_f
           else static_error "condBr operand not of type i1"));

update_stack ::
  LLVM_State.Frame ->
    LLVM_Syntax.Name -> LLVM_State.Stack_value -> LLVM_State.Frame;
update_stack f n o =
  LLVM_State.update_stack (\ _ -> Mapping.update n o (LLVM_State.stack f)) f;

inc_pos :: forall a b. (a, (b, Arith.Nat)) -> (a, (b, Arith.Nat));
inc_pos (fn, (bn, n)) = (fn, (bn, Arith.Suc n));

ret_from_frame ::
  LLVM_State.Llvm_state ->
    Action ->
      LLVM_State.Stack_value ->
        LLVM_State.Frame ->
          [LLVM_State.Frame] ->
            Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
ret_from_frame ls i c1 f fs =
  (case i of {
    Instruction (LLVM_Syntax.Assignment _ (LLVM_Syntax.Binop _ _ _)) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.Assignment _ (LLVM_Syntax.Select _ _ _)) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.Assignment _ (LLVM_Syntax.Icmp _ _ _)) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.Assignment n (LLVM_Syntax.R_Call _)) ->
      let {
        fa = LLVM_State.update_pos inc_pos (update_stack f n c1);
      } in Sum_Type.Inr (LLVM_State.update_frames (\ _ -> fa : fs) ls);
    Instruction (LLVM_Syntax.Assignment _ (LLVM_Syntax.Alloca _ _)) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.Assignment _ (LLVM_Syntax.Load _ _)) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.V_Call _) ->
      static_error "Illformed position information/program";
    Instruction (LLVM_Syntax.Store _ _ _) ->
      static_error "Illformed position information/program";
    Terminator _ -> static_error "Illformed position information/program";
  });

update_frame ::
  LLVM_State.Llvm_state ->
    [LLVM_State.Frame] ->
      LLVM_State.Frame -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
update_frame ls fs f =
  Sum_Type.Inr (LLVM_State.update_frames (\ _ -> f : fs) ls);

find_action ::
  LLVM_Syntax.Basic_block -> Arith.Nat -> Sum_Type.Sum LLVM_State.Stuck Action;
find_action b n =
  (if Arith.less_nat n (Impl.size_list (LLVM_Syntax.phis b))
    then static_error "jumping to phi node not possible"
    else (case LLVM_State.nth_option (LLVM_Syntax.instructions b)
                 (Arith.minus_nat n (Impl.size_list (LLVM_Syntax.phis b)))
           of {
           Nothing -> Sum_Type.Inr (Terminator (LLVM_Syntax.terminator b));
           Just i -> Sum_Type.Inr (Instruction i);
         }));

find_statement ::
  LLVM_Syntax.Llvm_prog ->
    (LLVM_Syntax.Name, (LLVM_Syntax.Name, Arith.Nat)) ->
      Sum_Type.Sum LLVM_State.Stuck Action;
find_statement prog (fn, (bn, p)) =
  Error_Monad.bind (find_fun_block prog fn bn) (\ b -> find_action b p);

terminate_frame ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Llvm_state ->
      LLVM_State.Frame ->
        [LLVM_State.Frame] ->
          LLVM_Syntax.Terminator ->
            Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
terminate_frame lc ls cf fs t =
  (case t of {
    LLVM_Syntax.Ret Nothing -> static_error "ret void not yet supported";
    LLVM_Syntax.Ret (Just o1) ->
      Error_Monad.bind (operand_value cf o1)
        (\ c ->
          Error_Monad.bind (case fs of {
                             [] -> Sum_Type.Inl (LLVM_State.ReturnValue c);
                             f : fsa -> Sum_Type.Inr (f, fsa);
                           })
            (\ (f, fsa) ->
              Error_Monad.bind (find_statement lc (LLVM_State.pos f))
                (\ i -> ret_from_frame ls i c f fsa)));
    LLVM_Syntax.CondBr c n1 n2 ->
      Error_Monad.bind (operand_value cf c)
        (\ ca ->
          Error_Monad.bind (condBr_to_frame ca n1 n2)
            (\ n_id ->
              Error_Monad.bind (update_bid_frame lc cf n_id)
                (update_frame ls fs)));
    LLVM_Syntax.Br n1 ->
      Error_Monad.bind (update_bid_frame lc cf n1) (update_frame ls fs);
  });

update_frames_stack ::
  LLVM_State.Llvm_state ->
    LLVM_State.Frame ->
      [LLVM_State.Frame] ->
        LLVM_Syntax.Name ->
          Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value ->
            Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
update_frames_stack ls cf fs n v =
  Error_Monad.bind v
    (\ o ->
      let {
        f = update_stack cf n o;
      } in (case LLVM_State.pos cf of {
             (nf, (nb, p)) ->
               update_frame ls fs
                 (LLVM_State.update_pos
                   (\ _ -> (nf, (nb, Arith.plus_nat p Arith.one_nat))) f);
           }));

binop_instruction ::
  LLVM_Syntax.Binop_instruction -> Arith.Int -> Arith.Int -> Arith.Int;
binop_instruction LLVM_Syntax.Xor = Bits_Int.bitXOR_int;
binop_instruction LLVM_Syntax.Mul = Arith.times_int;
binop_instruction LLVM_Syntax.Sub = Arith.minus_int;
binop_instruction LLVM_Syntax.Add = Arith.plus_int;

select_llvm_constant ::
  LLVM_State.Stack_value ->
    LLVM_State.Stack_value ->
      LLVM_State.Stack_value ->
        Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value;
select_llvm_constant (LLVM_State.IntegerValue a n) o1 o2 =
  Sum_Type.Inr (if Arith.equal_int n Arith.one_int then o1 else o2);
select_llvm_constant (LLVM_State.Pointer v va vb) uv uw =
  static_error "ill-typed";

operand_select ::
  LLVM_State.Frame ->
    LLVM_Syntax.Operand ->
      LLVM_Syntax.Operand ->
        LLVM_Syntax.Operand ->
          Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value;
operand_select cf c o1 o2 =
  let {
    _ = LLVM_State.stack cf;
  } in Error_Monad.bind (operand_value cf c)
         (\ ca ->
           Error_Monad.bind (operand_value cf o1)
             (\ o1a ->
               Error_Monad.bind (operand_value cf o2)
                 (select_llvm_constant ca o1a)));

operand_binop ::
  forall a.
    LLVM_State.Frame ->
      LLVM_Syntax.Operand ->
        LLVM_Syntax.Operand ->
          (LLVM_State.Stack_value ->
            LLVM_State.Stack_value -> Sum_Type.Sum LLVM_State.Stuck a) ->
            Sum_Type.Sum LLVM_State.Stuck a;
operand_binop cf o1 o2 bo =
  let {
    _ = LLVM_State.stack cf;
  } in Error_Monad.bind (operand_value cf o1)
         (\ o1a -> Error_Monad.bind (operand_value cf o2) (bo o1a));

map_of_funs ::
  LLVM_Syntax.Llvm_prog -> LLVM_Syntax.Name -> Maybe LLVM_Syntax.Llvm_fun;
map_of_funs lc =
  Map.map_of (map (\ f -> (LLVM_Syntax.fun_name f, f)) (LLVM_Syntax.funs lc));

mapping_impl_name :: Phantom_Type.Phantom LLVM_Syntax.Name Mapping.Mapping_impl;
mapping_impl_name = Phantom_Type.Phantom Mapping.Mapping_RBT;

zip_parameters ::
  LLVM_State.Frame ->
    [LLVM_Syntax.Operand] ->
      [LLVM_Syntax.Parameter] ->
        Sum_Type.Sum LLVM_State.Stuck
          (Mapping.Mapping LLVM_Syntax.Name LLVM_State.Stack_value);
zip_parameters cf (v : va) [] = static_error "Wrong number of arguments";
zip_parameters cf [] (v : va) = static_error "Wrong number of arguments";
zip_parameters cf [] [] =
  Sum_Type.Inr
    (Mapping.mapping_empty (Phantom_Type.of_phantom mapping_impl_name));
zip_parameters cf (x : xs) (LLVM_Syntax.Parameter t n : ps) =
  Error_Monad.bind (zip_parameters cf xs ps)
    (\ s ->
      Error_Monad.bind (operand_value cf x)
        (\ y -> Sum_Type.Inr (Mapping.update n y s)));

enter_frame ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Frame ->
      LLVM_Syntax.Name ->
        [LLVM_Syntax.Operand] -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Frame;
enter_frame lc cf n os =
  (case map_of_funs lc n of {
    Nothing -> static_error "Undefined function";
    Just (LLVM_Syntax.Function _ na ps b _) ->
      Error_Monad.bind (zip_parameters cf os ps)
        (\ s ->
          Sum_Type.Inr
            (LLVM_State.Frame (na, (LLVM_Syntax.name b, Arith.Zero_nat)) s
              LLVM_State.empty_mem));
    Just (LLVM_Syntax.ExternalFunction _ fn _) ->
      Sum_Type.Inl (LLVM_State.ExternalFunctionCall fn);
  });

call_function ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Llvm_state ->
      LLVM_State.Frame ->
        [LLVM_State.Frame] ->
          LLVM_Syntax.Name ->
            [LLVM_Syntax.Operand] ->
              Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
call_function lc ls cf fs fn os =
  (case map_of_funs lc fn of {
    Nothing -> static_error "Undefined function";
    Just (LLVM_Syntax.Function _ fna _ _ _) ->
      Error_Monad.bind (enter_frame lc cf fna os)
        (\ f ->
          Sum_Type.Inr (LLVM_State.update_frames (\ _ -> f : cf : fs) ls));
    Just (LLVM_Syntax.ExternalFunction _ fna _) ->
      Sum_Type.Inl (LLVM_State.ExternalFunctionCall fna);
  });

allocate ::
  LLVM_State.Frame ->
    LLVM_Syntax.Name ->
      LLVM_Syntax.Llvm_type ->
        Maybe LLVM_Syntax.Operand ->
          Sum_Type.Sum LLVM_State.Stuck LLVM_State.Frame;
allocate cf n t o1 =
  Error_Monad.bind
    (case o1 of {
      Nothing ->
        Sum_Type.Inr
          (LLVM_State.IntegerValue
            (Arith.nat_of_num
              (Arith.Bit0
                (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One))))))
            Arith.one_int);
      Just a -> operand_value cf a;
    })
    (\ l ->
      Error_Monad.bind
        (LLVM_State.malloc (LLVM_State.mem cf) t (LLVM_State.intValue l))
        (\ (a, mem) ->
          let {
            p = LLVM_State.Pointer t a Arith.Zero_nat;
          } in Sum_Type.Inr
                 (LLVM_State.update_stack (Mapping.update n p)
                   (LLVM_State.update_mem (\ _ -> mem) cf))));

binop_llvm_constant ::
  (Arith.Int -> Arith.Int -> Arith.Int) ->
    LLVM_State.Stack_value ->
      LLVM_State.Stack_value ->
        Sum_Type.Sum LLVM_State.Stuck LLVM_State.Stack_value;
binop_llvm_constant g (LLVM_State.IntegerValue aa na)
  (LLVM_State.IntegerValue a n) =
  (if Arith.equal_nat aa a && Arith.less_nat Arith.Zero_nat aa
    then Sum_Type.Inr (LLVM_State.IntegerValue aa (g na n))
    else static_error "Constant have different types");
binop_llvm_constant uu (LLVM_State.Pointer v va vb) uw =
  static_error "ill-typed";
binop_llvm_constant uu uv (LLVM_State.Pointer v va vb) =
  static_error "ill-typed";

icmp_llvm_constant ::
  LLVM_Syntax.IntegerPredicate ->
    LLVM_State.Stack_value -> LLVM_State.Stack_value -> LLVM_State.Stack_value;
icmp_llvm_constant LLVM_Syntax.EQ c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if Arith.equal_int (LLVM_State.intValue c1) (LLVM_State.intValue c2)
      then Arith.one_int else Arith.Zero_int);
icmp_llvm_constant LLVM_Syntax.NE c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if not (Arith.equal_int (LLVM_State.intValue c1) (LLVM_State.intValue c2))
      then Arith.one_int else Arith.Zero_int);
icmp_llvm_constant LLVM_Syntax.SLT c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if Arith.less_int (LLVM_State.intValue c1) (LLVM_State.intValue c2)
      then Arith.one_int else Arith.Zero_int);
icmp_llvm_constant LLVM_Syntax.SGT c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if Arith.less_int (LLVM_State.intValue c2) (LLVM_State.intValue c1)
      then Arith.one_int else Arith.Zero_int);
icmp_llvm_constant LLVM_Syntax.SGE c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if Arith.less_eq_int (LLVM_State.intValue c2) (LLVM_State.intValue c1)
      then Arith.one_int else Arith.Zero_int);
icmp_llvm_constant LLVM_Syntax.SLE c1 c2 =
  LLVM_State.IntegerValue Arith.one_nat
    (if Arith.less_eq_int (LLVM_State.intValue c1) (LLVM_State.intValue c2)
      then Arith.one_int else Arith.Zero_int);

store ::
  LLVM_State.Frame ->
    LLVM_Syntax.Operand ->
      LLVM_Syntax.Operand ->
        Sum_Type.Sum LLVM_State.Stuck [[Maybe LLVM_State.Stack_value]];
store cf o1 op =
  Error_Monad.bind (operand_value cf o1)
    (\ v ->
      Error_Monad.bind (operand_value cf op)
        (\ p ->
          let {
            a = (case p of {
                  LLVM_State.Pointer _ a b -> (a, b);
                });
          } in (case a of {
                 (aa, offs) ->
                   Error_Monad.bind
                     (LLVM_State.mem_store (LLVM_State.mem cf) aa offs v)
                     Sum_Type.Inr;
               })));

load ::
  LLVM_State.Frame ->
    LLVM_Syntax.Name ->
      LLVM_Syntax.Operand -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Frame;
load cf n op =
  Error_Monad.bind (operand_value cf op)
    (\ p ->
      let {
        a = (case p of {
              LLVM_State.Pointer _ a b -> (a, b);
            });
      } in (case a of {
             (aa, offs) ->
               Error_Monad.bind
                 (LLVM_State.mem_load (LLVM_State.mem cf) aa offs)
                 (\ v -> Sum_Type.Inr (update_stack cf n v));
           }));

run_instruction ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Llvm_state ->
      LLVM_State.Frame ->
        [LLVM_State.Frame] ->
          LLVM_Syntax.Instruction ->
            Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
run_instruction lc ls cf fs i =
  (case i of {
    LLVM_Syntax.Assignment n ia ->
      let {
        g = (\ o1 o2 h ->
              update_frames_stack ls cf fs n (operand_binop cf o1 o2 h));
      } in (case ia of {
             LLVM_Syntax.Binop binop o1 o2 ->
               g o1 o2 (binop_llvm_constant (binop_instruction binop));
             LLVM_Syntax.Select c o1 o2 ->
               update_frames_stack ls cf fs n (operand_select cf c o1 o2);
             LLVM_Syntax.Icmp c o1 o2 ->
               g o1 o2 (\ i1 i2 -> Sum_Type.Inr (icmp_llvm_constant c i1 i2));
             LLVM_Syntax.R_Call a ->
               (case a of {
                 LLVM_Syntax.Call _ aa b -> call_function lc ls cf fs aa b;
               });
             LLVM_Syntax.Alloca t o1 ->
               Error_Monad.bind (allocate cf n t o1)
                 (\ f -> update_frame ls fs (LLVM_State.update_pos inc_pos f));
             LLVM_Syntax.Load _ o1 ->
               Error_Monad.bind (load cf n o1)
                 (\ f -> update_frame ls fs (LLVM_State.update_pos inc_pos f));
           });
    LLVM_Syntax.V_Call _ -> static_error "Unnamed operation not yet supported";
    LLVM_Syntax.Store _ o1 p ->
      Error_Monad.bind
        (Error_Monad.bind (store cf o1 p)
          (\ m -> Sum_Type.Inr (LLVM_State.update_mem (\ _ -> m) cf)))
        (\ f -> update_frame ls fs (LLVM_State.update_pos inc_pos f));
  });

step ::
  LLVM_Syntax.Llvm_prog ->
    LLVM_State.Llvm_state ->
      Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
step lf ls =
  (case LLVM_State.frames ls of {
    [] -> Sum_Type.Inl LLVM_State.Program_Termination;
    f : fs ->
      (case find_statement lf (LLVM_State.pos f) of {
        Sum_Type.Inl _ -> static_error "Can\'t find next instruction";
        Sum_Type.Inr a -> (case a of {
                            Instruction aa -> run_instruction lf ls f fs aa;
                            Terminator aa -> terminate_frame lf ls f fs aa;
                          });
      });
  });

step_by_step ::
  Arith.Nat ->
    LLVM_Syntax.Llvm_prog ->
      LLVM_State.Llvm_state ->
        Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
step_by_step (Arith.Suc i) lc ls = (case step lc ls of {
                                     Sum_Type.Inl a -> Sum_Type.Inl a;
                                     Sum_Type.Inr a -> step_by_step i lc a;
                                   });
step_by_step Arith.Zero_nat lc ls = Sum_Type.Inr ls;

run_prog ::
  LLVM_Syntax.Llvm_prog ->
    String -> Arith.Nat -> Sum_Type.Sum LLVM_State.Stuck LLVM_State.Llvm_state;
run_prog p s n =
  let {
    first_block_in_main =
      (case map_of_funs p (LLVM_Syntax.Name s) of {
        Just (LLVM_Syntax.Function _ _ _ b _) -> LLVM_Syntax.name b;
      });
  } in step_by_step n p
         (LLVM_State.Llvm_state
           [LLVM_State.Frame
              (LLVM_Syntax.Name s, (first_block_in_main, Arith.Zero_nat))
              (Mapping.mapping_empty
                (Phantom_Type.of_phantom mapping_impl_name))
              LLVM_State.empty_mem]
           LLVM_State.empty_mem);

run_prog_string :: Integer -> String -> String -> String;
run_prog_string n s f =
  Shows_Literal.showsl_sum
    (run_prog (LLVM_Parser.parse_llvm s) f (Arith.nat_of_integer n)) "";

}
