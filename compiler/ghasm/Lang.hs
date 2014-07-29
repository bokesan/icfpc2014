module Lang (Stmt(..), Cond(..), expandArgs) where

import Ghc

data Stmt = SUnOp String Arg
          | SBinOp String Arg Arg
          | SInt Int
          | SIf Cond [Stmt] [Stmt]
          | SWhile Cond [Stmt]
          | SDoWhile [Stmt] Cond
          | SHalt
          deriving (Show)

data Cond = Cond String Arg Arg deriving (Show)

expandArgs :: [(String, Arg)] -> Stmt -> Stmt
expandArgs bs (SUnOp op a) = SUnOp op (expandArg bs a)
expandArgs bs (SBinOp op a b) = SBinOp op (expandArg bs a) (expandArg bs b)
expandArgs bs (SIf c b1 b2) = SIf (expandCond bs c)
                                  (map (expandArgs bs) b1)
                                  (map (expandArgs bs) b2)
expandArgs bs (SWhile c body) = SWhile (expandCond bs c) (map (expandArgs bs) body)
expandArgs bs (SDoWhile body c) = SDoWhile (map (expandArgs bs) body) (expandCond bs c)
expandArgs _ s = s

expandArg :: [(String, Arg)] -> Arg -> Arg
expandArg bs a@(Reg s) = case lookup s bs of
                           Nothing -> a
                           Just b -> b
expandArg _ a = a

expandCond :: [(String, Arg)] -> Cond -> Cond
expandCond bs (Cond op a b) = Cond op (expandArg bs a) (expandArg bs b)