module Compile (compileProgram) where

import Ghc
import Lang

compileProgram :: [Stmt] -> [Instruction]
compileProgram ds = relocate (cStmts () ds)

cStmts :: () -> [Stmt] -> [Instruction]
cStmts _ sts = concat (map (cStmt ()) sts)

cStmt :: () -> Stmt -> [Instruction]
cStmt _   SHalt      = [I_HLT]
cStmt _   (SUnOp "dec" a) = [I_DEC a]
cStmt _   (SUnOp "inc" a) = [I_INC a]
cStmt _   (SBinOp "mov" a b) = [I_MOV a b]
cStmt _   (SBinOp "add" a b) = [I_ADD a b]
cStmt _   (SBinOp "sub" a b) = [I_SUB a b]
cStmt _   (SBinOp "mul" a b) = [I_MUL a b]
cStmt _   (SBinOp "div" a b) = [I_DIV a b]
cStmt _   (SBinOp "and" a b) = [I_AND a b]
cStmt _   (SBinOp "or"  a b) = [I_OR  a b]
cStmt _   (SBinOp "xor" a b) = [I_XOR a b]
cStmt _   (SInt i) = [I_INT i]
cStmt env (SIf cond is []) = let is' = cStmts env is in
                             reverseCond cond (length is' + 1) : is'
cStmt env (SIf cond a b) =
     let a' = cStmts env a
         b' = cStmts env b
     in
         reverseCond cond (length a' + 2)
         : a' ++ [jump (length b' + 1)] ++ b'
cStmt env (SDoWhile is cond) = let is' = cStmts env is in
                               is' ++ [bCond cond (length is')]
cStmt env (SWhile cond is)   = let is' = cStmts env is in
                               reverseCond cond (length is' + 2)
                               : is' ++ [jump (- (length is' + 1))]

jump :: Int -> Instruction
jump n = I_JEQ n (Imm 0) (Imm 0)

reverseCond :: Cond -> Int -> Instruction
reverseCond (Cond "<=" a b) n = I_JGT n a b
reverseCond (Cond "/=" a b) n = I_JEQ n a b
reverseCond (Cond ">=" a b) n = I_JLT n a b
reverseCond (Cond op _ _) _   = error ("if/while: unsupported operator: " ++ op)

bCond :: Cond -> Int -> Instruction
bCond (Cond "==" a b) n = I_JEQ (-n) a b
bCond (Cond "<" a b) n = I_JLT (-n) a b
bCond (Cond ">" a b) n = I_JGT (-n) a b
bCond (Cond op _ _) _   = error ("do: unsupported operator: " ++ op)
