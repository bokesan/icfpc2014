module Compile (compileDefns) where

import Prelude hiding (uncurry)
import Control.Monad.State
import Expr
import Gcc

-- compile top-level expressions
compileDefns :: [(String, Expr)] -> [GccFunction String]
compileDefns defs = reverse (eFunctions st)
  where
    st = execState (go (tlEnv defs) defs) (CompState 0 [])
    go env ((name,e):ds) = do code <- cExpr env e
                              let f = GccFunction name (code ++ [I_RTN])
                              modify (addFunction f)
                              go env ds
    go _ [] = return ()

type Comp = State CompState

genName :: Comp String
genName = state (tFlip newName)

addFunction :: GccFunction String -> CompState -> CompState
addFunction f st = st{eFunctions = f : eFunctions st}

anonBlock :: [Instruction String] -> Comp String
anonBlock code = do name <- genName
                    modify (addFunction (GccFunction name code))
                    return name

tFlip :: (a -> (b,c)) -> a -> (c,b)
tFlip f a = let (b,c) = f a in (c,b)


-- Environments
data CompState = CompState {
                   eNs :: !Int {- Name supply -},
                   eFunctions :: [GccFunction String] {- If alternatives -}
                 }

newtype Env = Env [[(String, Type)]] {- lexical environment -}

data Type = Local | Global Int

tlEnv :: [(String, Expr)] -> Env
tlEnv ds =  Env [[(n, Global (numArgs e)) | (n,e) <- ds, isUser n]]
  where
    isUser ('_':_) = False
    isUser _ = True

newName :: CompState -> (CompState, String)
newName e = let ctr = eNs e
                name = "_" ++ show ctr
            in (e{eNs=ctr + 1}, name)

makeFrame :: Env -> [String] -> Env
makeFrame (Env e) ns = Env (map (\n -> (n, Local)) ns : e)

lookupE :: String -> Env -> (Int,Int,Type)
lookupE x (Env env) = go 0 env
  where
    go _ [] = error ("unbound name: " ++ x)
    go d (f:fs) = case look x f of
                    Nothing -> go (d+1) fs
                    Just (i,t) -> (d,i,t)

look :: (Eq a) => a -> [(a,b)] -> Maybe (Int,b)
look x xs = go 0 xs
 where
   go _ [] = Nothing
   go i ((z,t):zs) | x == z = Just (i,t)
                   | otherwise = go (i+1) zs

cBUILTIN_OPS :: [(String, Instruction String)]
cBUILTIN_OPS =  [("cons", I_CONS),
                 ("+", I_ADD), ("-", I_SUB),
                 ("*", I_MUL), ("/", I_DIV),
                 ("=", I_CEQ), (">", I_CGT), (">=", I_CGTE)]

cBUILTIN_FUNCTIONS :: [(String, Instruction String)]
cBUILTIN_FUNCTIONS =  [("car", I_CAR), ("cdr", I_CDR), ("atom", I_ATOM),
                       ("brk", I_BRK), ("dbug", I_DBUG)]


cExpr :: Env -> Expr -> Comp [Instruction String]

cExpr _   CNil          = return [I_LDC 0]
cExpr _   (CFun f)      = case lookup f cBUILTIN_OPS of
                            Just instr -> return [instr]
                            Nothing -> fail ("unknown instruction: " ++ f)
cExpr _   (CNum n)      = return [I_LDC (read n)]

cExpr env e@(EApp _ _)  = cApp env (gApp e)
cExpr env (EIf e c a)   = do ce <- cExpr env e
                             cc <- cExpr env c
                             ca <- cExpr env a
                             cLbl <- anonBlock (cc ++ [I_JOIN])
                             aLbl <- anonBlock (ca ++ [I_JOIN])
                             return (ce ++ [I_SEL cLbl aLbl])

cExpr env e@(ELam _ _)  = let (args, body) = uncurry e
                          in cExpr (makeFrame env args) body

cExpr env (EVar x)      =
  case lookup x cBUILTIN_FUNCTIONS of
    Just instr -> return [instr]
    Nothing    ->
      case lookupE x env of
        (n,i,Local)    -> return [I_LD n i x]
        (_,_,Global 0) -> return [I_LDF x x, I_AP 0]
        (_,_,Global _) -> return [I_LDF x x]

cExpr env (ELet ds e)   = do vals <- cExprs env (map snd ds)
                             body <- cExpr (makeFrame env (map fst ds)) e
                             lbl  <- anonBlock (body ++ [I_RTN])
                             return (vals ++ [I_DUM (length ds), I_LDF lbl lbl, I_RAP (length ds)])

numArgs :: Expr -> Int
numArgs e = length (fst (uncurry e))

uncurry :: Expr -> ([String], Expr)
uncurry (ELam x e) = let (ns,body) = uncurry e
                     in (x:ns, body)
uncurry e          = ([],e)


cApp :: Env -> [Expr] -> Comp [Instruction String]
cApp env [CFun "&&", e1, e2] = compileAnd env e1 e2
cApp env [CFun "||", e1, e2] = compileOr env e1 e2
cApp env (f:args) = do code <- cExprs env args
                       fc <- cExpr env f
                       let a = arity env f
                       if a /= length args
                         then fail ("wrong number of arguments to function: " ++ show f)
                         else if needsAP fc
                           then return (code ++ fc ++ [I_AP (length args)])
                           else return (code ++ fc)

-- e1 && e2 ==> if e1 then e2 else 0
compileAnd :: Env -> Expr -> Expr -> Comp [Instruction String]
compileAnd env e1 e2 = cExpr env (EIf e1 e2 (CNum "0"))

-- e1 || e2 ==> if e1 then 1 else e2
compileOr :: Env -> Expr -> Expr -> Comp [Instruction String]
compileOr env e1 e2 = cExpr env (EIf e1 (CNum "1") e2)

needsAP :: [Instruction addr] -> Bool
needsAP [I_LD _ _ _] = True
needsAP [I_LDF _ _] = True
needsAP _ = False

arity :: Env -> Expr -> Int
arity _   (CFun c) = case lookup c cBUILTINS of
                       Nothing -> error ("unknown builtin function: " ++ c)
                       Just a -> a
arity env (EVar x) = case lookup x cBUILTINS of
                       Just a -> a
                       Nothing -> case lookupE x env of
                                    (_,_,Local) -> 0
                                    (_,_,Global a) -> a


cBUILTINS :: [(String,Int)]
cBUILTINS =  [("car", 1), ("cdr", 1), ("cons", 2),
              ("+", 2), ("-", 2), ("*", 2), ("/", 2),
              ("=", 2), (">", 2), (">=", 2),
              ("atom", 1),
              ("brk", 1), ("dbug", 2),
              ("&&", 2), ("||", 2)]

gApp :: Expr -> [Expr]
gApp (EApp e1 e2) = gApp e1 ++ [e2]
gApp e            = [e]

cExprs :: Env -> [Expr] -> Comp [Instruction String]
cExprs env es = do iss <- mapM (cExpr env) es
                   return $ concat iss
