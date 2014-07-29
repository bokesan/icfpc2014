module Expr (
        Expr(..),
        Defn, Id,
        mkApp, arg, fun, complexity
) where


import Prelude hiding (const, maybe)
import Data.Char (isLower, isAlpha)


type Id		=  String

data Expr	=  CFun Id	-- Combinators
		|  CNil
		|  CPair Expr Expr
		|  CNum String
		|  EVar Id
		|  ELam Id Expr
		|  EApp Expr Expr
                |  EIf Expr Expr Expr
		|  ELet [Defn] Expr
		|  ELetrec [Defn] Expr
		deriving (Eq, Ord)

instance Show Expr where
  showsPrec _ e = displayExpr False e

type Defn	=  (Id, Expr)

complexity		:: Expr -> (Int, Int)
complexity e		=  (size e, nesting e)

size			:: Expr -> Int
size (EApp f a)		=  size f + size a
size _			=  1

nesting			:: Expr -> Int
nesting (EApp f a)	=  max (nesting f) (nesting a + 1)
nesting _		=  0


arg, fun		:: Expr -> Expr
fun (EApp f _)		=  f
fun _			=  error "Expr.fun: not an application"
arg (EApp _ a)		=  a
arg _			=  error "Expr.arg: not an application"

mkApp			:: [Expr] -> Expr
mkApp []		=  error "Expr.mkApp: empty list"
mkApp es		=  foldl1 EApp es

displayDefn		:: Defn -> ShowS
displayDefn (x,e)	=  showChar '[' . shows s . showChar ',' . shows n
			   . showString "] "
			   . showString x
			   . showString " = "
			   . displayExpr False e
  where
    (s,n) = complexity e

displayExpr :: Bool -> Expr -> ShowS
displayExpr _ (CNum n)  = shows n
displayExpr _ CNil	= showString "[]"
displayExpr _ (CFun x@(c:_)) | isLower c = showChar '#' . showString x
                             | isSymbol x = showChar '(' . showString x . showChar ')'
                             | otherwise = showString x
displayExpr _ e@(CPair _ _) =
   let h CNil = id
       h (CPair hd tl) = displayExpr False hd . showString ", " . h tl
   in showChar '[' . h e . showChar ']'
displayExpr _ (EVar x) = if isSymbol x then showChar '(' . showString x . showChar ')'
                         else showString x
displayExpr p (EApp f a) =
   (if p then showChar '(' else id)
   . displayExpr False f
   . showChar ' '
   . displayExpr True a
   . (if p then showChar ')' else id)
displayExpr _ (ELet ds e) = showString "let\n" . displayLet ds e
displayExpr _ (ELetrec ds e) = showString "letrec\n" . displayLet ds e
displayExpr _ (ELam x e) = showString "(\\" . showString x . displayLambdaBody e
displayExpr _ (EIf e c a) = showString "if " . displayExpr False e . showString " then "
                          . displayExpr False c . showString " else " . displayExpr False a

displayLambdaBody :: Expr -> ShowS
displayLambdaBody (ELam x e) = showChar ' ' . showString x . displayLambdaBody e
displayLambdaBody e = showString " -> " . displayExpr False e . showChar ')'



displayLet :: [Defn] -> Expr -> ShowS
displayLet ds e =
   displayDefns ds . showString "\nin " . displayExpr True e


displayDefns :: [Defn] -> ShowS
displayDefns [] = id
displayDefns [d] = showString "   " . displayDefn d
displayDefns (d:ds) = showString "   " . displayDefn d
	. showString ".\n" . displayDefns ds


isSymbol :: String -> Bool
isSymbol (c:_) = not (isAlpha c)


