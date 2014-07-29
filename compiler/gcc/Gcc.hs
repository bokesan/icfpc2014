module Gcc (Instruction(..), GccFunction(..), link) where


data Instruction addr = I_LDC Int            -- Load constant
                      | I_LD Int Int String  -- Load from Environment
                      | I_ADD | I_SUB | I_MUL | I_DIV
                      | I_CEQ | I_CGT | I_CGTE
                      | I_ATOM
                      | I_CONS | I_CAR | I_CDR
                      | I_SEL addr addr | I_JOIN
                      | I_LDF addr String
                      | I_AP Int
                      | I_RTN
                      | I_DUM Int
                      | I_RAP Int
                      -- I_STOP
                      | I_TSEL addr addr
                      | I_TAP Int
                      | I_TRAP Int
                      | I_ST Int Int
                      | I_DBUG | I_BRK

instance (Show addr) => Show (Instruction addr) where
  showsPrec _ (I_LDC n)    = showString "LDC " . shows n
  showsPrec _ (I_LD n i nm) = showString "LD " . shows n . showChar ' ' . shows i . showString " ; " . showString nm
  showsPrec _ I_ADD        = showString "ADD"
  showsPrec _ I_SUB        = showString "SUB"
  showsPrec _ I_MUL        = showString "MUL"
  showsPrec _ I_DIV        = showString "DIV"
  showsPrec _ I_CEQ        = showString "CEQ"
  showsPrec _ I_CGT        = showString "CGT"
  showsPrec _ I_CGTE       = showString "CGTE"
  showsPrec _ I_ATOM       = showString "ATOM"
  showsPrec _ I_CONS       = showString "CONS"
  showsPrec _ I_CAR        = showString "CAR"
  showsPrec _ I_CDR        = showString "CDR"
  showsPrec _ (I_SEL t f)  = showString "SEL " . shows t . showChar ' ' . shows f
  showsPrec _ I_JOIN       = showString "JOIN"
  showsPrec _ (I_LDF f nm) = showString "LDF " . shows f . showString " ; " . showString nm
  showsPrec _ (I_AP n)     = showString "AP " . shows n
  showsPrec _ I_RTN        = showString "RTN"
  showsPrec _ (I_DUM n)    = showString "DUM " . shows n
  showsPrec _ (I_RAP n)    = showString "RAP " . shows n
  showsPrec _ (I_TSEL t f) = showString "TSEL " . shows t . showChar ' ' . shows f
  showsPrec _ (I_TAP n)    = showString "TAP " . shows n
  showsPrec _ (I_TRAP n)   = showString "TRAP " . shows n
  showsPrec _ (I_ST n i)   = showString "ST " . shows n . showChar ' ' . shows i
  showsPrec _ I_DBUG       = showString "DBUG"
  showsPrec _ I_BRK        = showString "BRK"

linkInstr :: [(String,Int)] -> Instruction String -> Instruction Address
linkInstr addr (I_SEL a1 a2) = I_SEL (resolve a1 addr) (resolve a2 addr)
linkInstr addr (I_LDF a nm) = I_LDF (resolve a addr) nm
linkInstr addr (I_TSEL a1 a2) = I_TSEL (resolve a1 addr) (resolve a2 addr)
linkInstr _ I_JOIN = I_JOIN
linkInstr _ (I_LDC n) = I_LDC n
linkInstr _ (I_LD n i nm) = I_LD n i nm
linkInstr _ (I_ST n i) = I_ST n i
linkInstr _ I_ADD = I_ADD
linkInstr _ I_SUB = I_SUB
linkInstr _ I_MUL = I_MUL
linkInstr _ I_DIV = I_DIV
linkInstr _ I_CONS = I_CONS
linkInstr _ I_CAR = I_CAR
linkInstr _ I_CDR = I_CDR
linkInstr _ I_CEQ = I_CEQ
linkInstr _ I_CGT = I_CGT
linkInstr _ I_CGTE = I_CGTE
linkInstr _ I_ATOM = I_ATOM
linkInstr _ I_RTN = I_RTN
linkInstr _ I_DBUG = I_DBUG
linkInstr _ I_BRK = I_BRK
linkInstr _ (I_TAP n) = I_TAP n
linkInstr _ (I_TRAP n) = I_TRAP n
linkInstr _ (I_RAP n) = I_RAP n
linkInstr _ (I_AP n) = I_AP n
linkInstr _ (I_DUM n) = I_DUM n

data GccFunction addr = GccFunction addr [Instruction addr]

instance Show addr => Show (GccFunction addr) where
  -- showsPrec _ (GccFunction a c) = showString "; " . shows a . prefixedBy "\n  " c
  showsPrec _ (GccFunction _ c) = sepBy "\n" c

prefixedBy :: (Show a) => String -> [a] -> ShowS
prefixedBy _ [] = id
prefixedBy sep (x:xs) = showString sep . shows x . prefixedBy sep xs

sepBy :: (Show a) => String -> [a] -> ShowS
sepBy _ [] = id
sepBy _ [x] = shows x
sepBy sep (x:xs) = shows x . showString sep . sepBy sep xs 


link :: [GccFunction String] -> [GccFunction Address]
link fns = go fns
  where
    annAddr _ [] = []
    annAddr n (GccFunction name code : fs) = (name, n) : annAddr (n + length code) fs

    addr = annAddr 0 fns

    go xs = map linkFn xs

    linkFn (GccFunction name code) = GccFunction (resolve name addr) (linkCode code)
    linkCode code = map (linkInstr addr) code

    numDefns = length [ name | (GccFunction name _) <- fns, isUser name ]

    main = GccFunction (Address "main" 0)
                       ([I_DUM numDefns] ++
                        [I_LDF (resolve f addr) f | (GccFunction f _) <- fns, isUser f] ++
                        [I_LDF (Address "_main2" (numDefns + cMAIN2_OFFS)) "_main2",
                         I_RAP numDefns,
                         I_RTN,
                         I_LDF (resolve "init" addr) "init",
                         I_AP 2,
                         I_RTN])

cPRELUDE_LENGTH :: Int
cPRELUDE_LENGTH = cMAIN2_OFFS + 3

cMAIN2_OFFS :: Int
cMAIN2_OFFS = 4
           
isUser :: String -> Bool
isUser ('_' : _) = False
isUser _ = True
    
                
resolve :: String -> [(String, Int)] -> Address
resolve name addr = case lookup name addr of
  Just line -> Address name line
  Nothing -> error ("linker: unknown function: " ++ name)


data Address = Address String Int

instance Show Address where
  showsPrec _ (Address _ line) = shows line
