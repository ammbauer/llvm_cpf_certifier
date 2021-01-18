{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  LLVM_Syntax(Name(..), equal_name, Llvm_constant(..), Frame(..), Named(..),
               Operand(..), Binop_instruction(..), IntegerPredicate(..),
               Llvm_type(..), Instruction(..), Terminator(..), Basic_block(..),
               Parameter(..), Llvm_fun(..), Llvm_prog(..), Llvm_state(..), pos,
               update_pos, stack, update_stack, update_frames, funs, params,
               name, phis, fun_name, frames, hd_blocks, tl_blocks, terminator,
               named_instruction, instructions, parameter_name, integerValue,
               equal_operand)
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
import qualified Mapping;
import qualified Arith;
import qualified Orders;

data Name = Name String | UnName Arith.Nat;

equal_name :: Name -> Name -> Bool;
equal_name (Name x1) (UnName x2) = False;
equal_name (UnName x2) (Name x1) = False;
equal_name (UnName x2) (UnName y2) = Arith.equal_nat x2 y2;
equal_name (Name x1) (Name y1) = x1 == y1;

instance Eq Name where {
  a == b = equal_name a b;
};

less_name :: Name -> Name -> Bool;
less_name (Name sa) (Name s) = sa < s;
less_name (Name s) (UnName n) = True;
less_name (UnName n) (Name s) = False;
less_name (UnName na) (UnName n) = Arith.less_nat na n;

less_eq_name :: Name -> Name -> Bool;
less_eq_name x y = equal_name x y || less_name x y;

instance Orders.Ord Name where {
  less_eq = less_eq_name;
  less = less_name;
};

instance Orders.Quasi_order Name where {
};

instance Orders.Weak_order Name where {
};

instance Orders.Preorder Name where {
};

instance Orders.Order Name where {
};

instance Orders.Linorder Name where {
};

data Llvm_constant = IntConstant Arith.Nat Arith.Int;

data Frame =
  Frame (Name, (Name, Arith.Nat)) (Mapping.Mapping Name Llvm_constant);

data Named a = Named Name a | Do a;

data Operand = LocalReference Name | ConstantOperand Llvm_constant;

data Binop_instruction = Add | Sub | Xor | Mul;

data IntegerPredicate = SLT | SGT | EQ | SGE | SLE | NE;

data Llvm_type = IntType Arith.Nat | VoidType;

data Instruction = Binop Binop_instruction Operand Operand
  | Select Operand Operand Operand | Call Llvm_type Name [Operand]
  | Icmp IntegerPredicate Operand Operand;

data Terminator = Ret (Maybe Operand) | CondBr Operand Name Name | Br Name;

data Basic_block =
  Basic_block Name [(Name, [(Operand, Name)])] [Named Instruction]
    (Named Terminator);

data Parameter = Parameter Llvm_type Name;

data Llvm_fun = Function Llvm_type Name [Parameter] Basic_block [Basic_block]
  | ExternalFunction Llvm_type Name [Parameter];

newtype Llvm_prog = Llvm_prog [Llvm_fun];

newtype Llvm_state = Llvm_state [Frame];

pos :: Frame -> (Name, (Name, Arith.Nat));
pos (Frame x1 x2) = x1;

update_pos ::
  ((Name, (Name, Arith.Nat)) -> (Name, (Name, Arith.Nat))) -> Frame -> Frame;
update_pos f (Frame p s) = Frame (f p) s;

stack :: Frame -> Mapping.Mapping Name Llvm_constant;
stack (Frame x1 x2) = x2;

update_stack ::
  (Mapping.Mapping Name Llvm_constant -> Mapping.Mapping Name Llvm_constant) ->
    Frame -> Frame;
update_stack f (Frame p s) = Frame p (f s);

update_frames :: ([Frame] -> [Frame]) -> Llvm_state -> Llvm_state;
update_frames f (Llvm_state fs) = Llvm_state (f fs);

funs :: Llvm_prog -> [Llvm_fun];
funs (Llvm_prog x) = x;

params :: Llvm_fun -> [Parameter];
params (Function x11 x12 x13 x14 x15) = x13;
params (ExternalFunction x21 x22 x23) = x23;

name :: Basic_block -> Name;
name (Basic_block x1 x2 x3 x4) = x1;

phis :: Basic_block -> [(Name, [(Operand, Name)])];
phis (Basic_block x1 x2 x3 x4) = x2;

fun_name :: Llvm_fun -> Name;
fun_name (Function x11 x12 x13 x14 x15) = x12;
fun_name (ExternalFunction x21 x22 x23) = x22;

frames :: Llvm_state -> [Frame];
frames (Llvm_state x) = x;

hd_blocks :: Llvm_fun -> Basic_block;
hd_blocks (Function x11 x12 x13 x14 x15) = x14;

tl_blocks :: Llvm_fun -> [Basic_block];
tl_blocks (Function x11 x12 x13 x14 x15) = x15;

terminator :: Basic_block -> Named Terminator;
terminator (Basic_block x1 x2 x3 x4) = x4;

named_instruction :: forall a. Named a -> a;
named_instruction (Named x11 x12) = x12;
named_instruction (Do x2) = x2;

instructions :: Basic_block -> [Named Instruction];
instructions (Basic_block x1 x2 x3 x4) = x3;

parameter_name :: Parameter -> Name;
parameter_name (Parameter x1 x2) = x2;

integerValue :: Llvm_constant -> Arith.Int;
integerValue (IntConstant x1 x2) = x2;

equal_llvm_constant :: Llvm_constant -> Llvm_constant -> Bool;
equal_llvm_constant (IntConstant x1 x2) (IntConstant y1 y2) =
  Arith.equal_nat x1 y1 && Arith.equal_int x2 y2;

equal_operand :: Operand -> Operand -> Bool;
equal_operand (LocalReference x1) (ConstantOperand x2) = False;
equal_operand (ConstantOperand x2) (LocalReference x1) = False;
equal_operand (ConstantOperand x2) (ConstantOperand y2) =
  equal_llvm_constant x2 y2;
equal_operand (LocalReference x1) (LocalReference y1) = equal_name x1 y1;

}
