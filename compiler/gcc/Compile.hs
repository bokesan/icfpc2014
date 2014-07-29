module Compile (compileDefns) where

import Prelude hiding (uncurry)
import Expr
import Gcc

-- compile top-level expressions
compileDefns :: [(String, Expr)] -> [GccFunction String]
compileDefns defs = go (tlEnv defs) defs
  where
    go env [] = reverse (eFunctions env)
    go env ((name,e):ds) = let (env',code) = cExpr env e
                               f = GccFunction name (code ++ [I_RTN])
                           in go (env'{eFunctions = f : eFunctions env'}) ds

-- Environments
data Env = Env { eNs :: !Int {- Name supply -},
                 eFunctions :: [GccFunction String] {- If alternatives -},
                 eLex :: [[(String, Type)]] {- lexical environment -}
               }

data Type = Local | Global Int

tlEnv :: [(String, Expr)] -> Env
tlEnv ds =  Env 0 [] [[(n, Global (numArgs e)) | (n,e) <- ds, isUser n]]
  where
    isUser ('_':_) = False
    isUser _ = True

newName :: Env -> (Env, String)
newName e = let ctr = eNs e
                name = "_" ++ show ctr
            in (e{eNs=ctr + 1}, name)

anonFunction :: Env -> [Instruction String] -> (Env, String)
anonFunction env code = let (env', name) = newName env
                            f = GccFunction name code
                        in (env'{eFunctions = f : eFunctions env'}, name)

makeFrame :: Env -> [String] -> Env
makeFrame e ns = e{eLex = map (\n -> (n, Local)) ns : eLex e}

lookupE :: String -> Env -> (Int,Int,Type)
lookupE x env = go 0 (eLex env)
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

cExpr :: Env -> Expr -> (Env, [Instruction String])

cExpr env CNil          = (env, [I_LDC 0])

cExpr env (CFun "cons") = (env, [I_CONS])
cExpr env (CFun "+")    = (env, [I_ADD])
cExpr env (CFun "-")    = (env, [I_SUB])
cExpr env (CFun "*")    = (env, [I_MUL])
cExpr env (CFun "/")    = (env, [I_DIV])
cExpr env (CFun "=")    = (env, [I_CEQ])
cExpr env (CFun ">")    = (env, [I_CGT])
cExpr env (CFun ">=")   = (env, [I_CGTE])

cExpr env (CNum n) = (env, [I_LDC (read n)])

cExpr env e@(EApp _ _) = cApp env (gApp e)

cExpr env (EIf e c a) =
  let (env', ce) = cExpr env e
      (env'', cc) = cExpr env' c
      (env''', ca) = cExpr env'' a
      (env4, cLbl) = anonFunction env''' (cc ++ [I_JOIN])
      (env5, aLbl) = anonFunction env4 (ca ++ [I_JOIN])
  in (env5, ce ++ [I_SEL cLbl aLbl])

cExpr env e@(ELam _ _) =
  let (args, body) = uncurry e
      (env', code) = cExpr (makeFrame env args) body
  in (env'{eLex = eLex env}, code)

cExpr env (EVar "car")   = (env, [I_CAR])
cExpr env (EVar "cdr")   = (env, [I_CDR])
cExpr env (EVar "atom")  = (env, [I_ATOM])
cExpr env (EVar "brk")   = (env, [I_BRK])
cExpr env (EVar "dbug")  = (env, [I_DBUG])
cExpr env (EVar x) = case lookupE x env of
                       (n,i,Local) -> (env, [I_LD n i x])
                       (_,_,Global 0) -> (env, [I_LDF x x, I_AP 0])
                       (_,_,Global _) -> (env, [I_LDF x x])

cExpr env (ELet ds e) = let (env', vars) = cExprs env (map snd ds)
                            (env'', code) = cExpr (makeFrame env' (map fst ds)) e
                            (env''', lbl) = anonFunction env'' (code ++ [I_RTN])
                        in (env'''{eLex = eLex env},
                            vars ++ [I_DUM (length ds), I_LDF lbl lbl, I_RAP (length ds)])
                           
cExprs :: Env -> [Expr] -> (Env, [Instruction String])
cExprs env [] = (env, [])
cExprs env (e:es) = let (env', c1) = cExpr env e
                        (env'', cs) = cExprs env' es
                    in (env'', c1 ++ cs)


numArgs :: Expr -> Int
numArgs e = length (fst (uncurry e))

uncurry :: Expr -> ([String], Expr)
uncurry (ELam x e) = let (ns,body) = uncurry e
                     in (x:ns, body)
uncurry e          = ([],e)


cApp :: Env -> [Expr] -> (Env, [Instruction String])
cApp env [CFun "&&", e1, e2] = compileAnd env e1 e2
cApp env [CFun "||", e1, e2] = compileOr env e1 e2
cApp env (f:args) = let (e', code) = cArgs env args
                        (e'', fc) = cExpr e' f
                        a = arity env f
                    in
                       if a /= length args then
                         error ("wrong number of arguments for function: " ++ show f)
                       else if needsAP fc
                        then (e'', code ++ fc ++ [I_AP (length args)])
                        else (e'', code ++ fc)


-- e1 && e2 ==> if e1 then e2 else 0
compileAnd :: Env -> Expr -> Expr -> (Env, [Instruction String])
compileAnd env e1 e2 = cExpr env (EIf e1 e2 (CNum "0"))

-- e1 || e2 ==> if e1 then 1 else e2
compileOr :: Env -> Expr -> Expr -> (Env, [Instruction String])
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

cArgs :: Env -> [Expr] -> (Env, [Instruction String])
cArgs env [] = (env, [])
cArgs env (e1:es) = let (e', code) = cExpr env e1
                        (e'', cs) = cArgs e' es
                        in (e'', code ++ cs)
                           
