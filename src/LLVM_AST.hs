module LLVM_AST where
import LLVM.Internal.Module
import LLVM.Internal.Context
import Data.ByteString.Short (fromShort)
import Data.ByteString.Char8 (unpack)
import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Global as G
import Data.Maybe
import System.IO.Unsafe

import LLVM_Syntax
import Arith

isFunction (A.GlobalDefinition a@(A.Function _ _ _ _ _ _ _ _ _ _ _ _ _ _ bs _ _)) = Just a
isFunction _ = Nothing

toFun ff = case G.basicBlocks ff of
  b:bs ->
    Just $ Function VoidType (toName $ G.name ff) (map toParameter $ fst $ G.parameters ff) (toBasic_block b) (map toBasic_block $ bs)
  [] -> Just $ ExternalFunction VoidType (toName $ G.name ff) (map toParameter $ fst $ G.parameters ff)

toName (A.Name s) = Name $ unpack $ fromShort s
toName (A.UnName i) = UnName (nat_of_integer $ toInteger i)

toParameter (G.Parameter _ n _) = Parameter VoidType (toName n)

toLlvm_constant (C.GlobalReference _ n) = error "GlobalReferences not supported"
toLlvm_constant c@(C.Int l i) = IntConstant (nat_of_integer l') (Int_of_integer i')
  where l' = toInteger l
        i'' = C.signedIntegerValue c
        -- True is represented as -1 in the case of a 1-bit value whereas we
        -- (Aprove and our semantics) consider 1 as true
        i' = if l' == 1 && i'' == -1 then 1 else i''


toOperand (A.LocalReference t n) = LocalReference (toName n)
toOperand (A.ConstantOperand c) = ConstantOperand (toLlvm_constant c)
toOperand _ = LocalReference (Name "Unsupported Operand")

toPredicate (IP.EQ) =  LLVM_Syntax.EQ
toPredicate (IP.SLT) = SLT
toPredicate (IP.SGT) = SGT
toPredicate (IP.SGE) = SGE
toPredicate (IP.SLE) = SLE
toPredicate (IP.NE) = LLVM_Syntax.NE

toInstruction (A.Add _ _ o1 o2 _) = Just (Binop Add (toOperand o1) (toOperand o2))
toInstruction (A.Sub _ _ o1 o2 _) = Just (Binop Sub (toOperand o1) (toOperand o2))
toInstruction (A.Phi _ xs _) = Nothing
toInstruction (A.Mul _ _ o1 o2 _) = Just (Binop Mul (toOperand o1) (toOperand o2))
toInstruction (A.ICmp p o1 o2 _) = Just (Icmp (toPredicate p) (toOperand o1) (toOperand o2))
toInstruction (A.Select c o1 o2 _) = Just (Select (toOperand c) (toOperand o1) (toOperand o2))
toInstruction (A.Xor o1 o2 _) = Just (Binop Xor (toOperand o1) (toOperand o2))
toInstruction (A.Call _ _ _ (Right o1) as _ _) = Just (Call VoidType (functionName o1) (map (toOperand . fst) as))
toInstruction _ = Nothing

functionName (A.ConstantOperand (C.GlobalReference _ n)) = toName n
functionName _ = error "Reference to function not supported"

fromNameI (n A.:= i) = fmap (Named (toName n)) $ toInstruction i
fromNameI (A.Do i) = fmap Do $ toInstruction i

toTerminator (A.CondBr c n1 n2 _) = CondBr (toOperand c) (toName n1) (toName n2)
toTerminator (A.Br n _) = Br (toName n)
toTerminator (A.Ret (Just o1) _) = Ret $ Just $ toOperand o1
toTerminator _ = Br (Name $ "Unknown Terminator")

fromNameT (n A.:= t) = Named (toName n) $ toTerminator t
fromNameT (A.Do t) = Do $ toTerminator t

toBasic_block (A.BasicBlock nb is nt) = Basic_block (toName nb) phis (mapMaybe fromNameI is) (fromNameT nt)
  where
  phis = mapMaybe phiToPair is
  phiToPair (n A.:= I.Phi _ ps _) = Just (toName n, map (\(o, m) -> (toOperand o, toName m)) ps)
  phiToPair _ = Nothing

emptyBlock = Basic_block (UnName $ nat_of_integer 1) [] [] (Do $ Br (UnName $ nat_of_integer 1))

getFunctions m = Llvm_prog $ mapMaybe toFun $ mapMaybe isFunction $ A.moduleDefinitions m

parse_llvm_string s = getFunctions $ unsafePerformIO $ withContext (\c -> withModuleFromLLVMAssembly c s moduleAST)
