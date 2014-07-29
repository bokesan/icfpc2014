module Ghc where


data Arg = Reg String
         | Ind String
         | Imm Int
         | Mem Int
         deriving (Eq)

registerNames :: [String]
registerNames =  ["a","b","c","d","e","f","g","h","pc"]

instance Show Arg where
  showsPrec _ (Reg s) = showString s
  showsPrec _ (Ind s) = showChar '[' . showString s . showChar ']'
  showsPrec _ (Imm n) = shows n
  showsPrec _ (Mem n) = showChar '[' . shows n . showChar ']'

isValidArg :: Arg -> Bool
isValidArg (Reg s) = s `elem` registerNames
isValidArg (Ind s) = s `elem` registerNames
isValidArg (Imm n) = n >= 0 && n <= 255
isValidArg (Mem n) = n >= 0 && n <= 255

data Instruction = I_MOV Arg Arg
                 | I_INC Arg
                 | I_DEC Arg
                 | I_ADD Arg Arg
                 | I_SUB Arg Arg
                 | I_MUL Arg Arg
                 | I_DIV Arg Arg
                 | I_AND Arg Arg
                 | I_OR Arg Arg
                 | I_XOR Arg Arg
                 | I_JLT Int Arg Arg -- label, addr
                 | I_JEQ Int Arg Arg
                 | I_JGT Int Arg Arg
                 | I_INT Int
                 | I_HLT
                 deriving (Eq)

instance Show Instruction where
  showsPrec _ (I_MOV a b)  = op2 "mov" a b
  showsPrec _ (I_ADD a b)  = op2 "add" a b
  showsPrec _ (I_SUB a b)  = op2 "sub" a b
  showsPrec _ (I_MUL a b)  = op2 "mul" a b
  showsPrec _ (I_DIV a b)  = op2 "div" a b
  showsPrec _ (I_AND a b)  = op2 "and" a b
  showsPrec _ (I_OR  a b)  = op2 "or " a b
  showsPrec _ (I_XOR a b)  = op2 "xor" a b
  showsPrec _ I_HLT        = showString "hlt"
  showsPrec _ (I_INC a)    = showString "inc " . shows a
  showsPrec _ (I_DEC a)    = showString "dec " . shows a
  showsPrec _ (I_INT n)    = showString "int " . shows n
  showsPrec _ (I_JLT addr a b) = branch "lt" addr a b
  showsPrec _ (I_JEQ addr a b) = branch "eq" addr a b
  showsPrec _ (I_JGT addr a b) = branch "gt" addr a b

op2 :: String -> Arg -> Arg -> ShowS
op2 instr a b = showString instr . showChar ' ' . shows a . showChar ',' . shows b

branch :: String -> Int -> Arg -> Arg -> ShowS
branch cmp addr a b = showChar 'j' . showString cmp . showChar ' '
                    . shows addr
                    . showChar ',' . shows a . showChar ',' . shows b

relocate :: [Instruction] -> [Instruction]
relocate is = [reloc p | p <- zip [0 ..] is]

reloc :: (Int, Instruction) -> Instruction
reloc (addr, I_JLT offs a b) = I_JLT (addr + offs) a b
reloc (addr, I_JEQ offs a b) = I_JEQ (addr + offs) a b
reloc (addr, I_JGT offs a b) = I_JGT (addr + offs) a b
reloc (_, instr) = instr
