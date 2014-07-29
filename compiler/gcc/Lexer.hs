module Lexer (Lexeme(..), lex,
	      lIsIdent, lIsNum, lIsChar, lIsString, lIsBool) where

import Prelude hiding (lex)
import Data.Char

data  Lexeme	=  L_lparen | L_rparen
		|  L_lbracket | L_rbracket
		|  L_lbrace | L_rbrace
		|  L_hash | L_comma | L_num String
		|  L_mod | L_div | L_mul | L_plus | L_minus
		|  L_and | L_or | L_arrow | L_dot
		|  L_neq | L_colon | L_less | L_less_eq
		|  L_equals | L_eq | L_greater | L_gr_eq
		|  L_lambda | L_semi | L_plusplus
		|  L_string String | L_char Char | L_id String
		|  L_res String
		deriving (Eq, Show)

lIsIdent, lIsChar, lIsNum, lIsString, lIsBool :: Lexeme -> Bool

lIsIdent (L_id _) = True
lIsIdent _ = False

lIsChar (L_char _) = True
lIsChar _ = False

lIsNum (L_num _) = True
lIsNum _ = False

lIsString (L_string _) = True
lIsString _ = False

lIsBool (L_res "False") = True
lIsBool (L_res "True") = True
lIsBool _ = False

lex :: String -> [Lexeme]

lex inp = h (dropSpace inp)
   where
      h "" = []
      h s  = let (l,s') = next s in l : h (dropSpace s')

dropSpace :: String -> String
dropSpace s = dropWhile isSpace s

next :: String -> (Lexeme, String)
next ('"':s) = let (a,b) = scanString s in (L_string a, b)
next ('#':s) = (L_hash, s)
next ('%':s) = (L_mod, s)
next ('&':s) = scan2 s '&' L_and
next ('\'':s) = scanChar s
next ('(':s) = (L_lparen,s)
next (')':s) = (L_rparen,s)
next ('*':s) = (L_mul,s)
next ('+':s) = scanComp s '+' L_plus L_plusplus
next (',':s) = (L_comma,s)
next ('-':s) = case s of
		  '-':s' -> next (dropSpace (dropWhile ('\n' /=) s'))
		  '>':s' -> (L_arrow, s')
		  _      -> (L_minus, s)
next ('.':s) = (L_dot,s)
next ('/':s) = scanComp s '=' L_div L_neq
next (':':s) = (L_colon,s)
next (';':s) = (L_semi, s)
next ('<':s) = scanComp s '=' L_less L_less_eq
next ('=':s) = scanComp s '=' L_equals L_eq
next ('>':s) = scanComp s '=' L_greater L_gr_eq
next ('[':s) = (L_lbracket, s)
next (']':s) = (L_rbracket, s)
next ('\\':s) = (L_lambda, s)
next ('{':s) = (L_lbrace, s)
next ('}':s) = (L_rbrace, s)
next ('|':s) = scan2 s '|' L_or
next (c:s) | isAlpha c || c == '_' = scanIdentOrReserved c s
	   | isDigit c = scanNumber c s
	   | otherwise = error ("illegal character in input: " ++ show c)
next "" = error "Lexer.next"


scanComp :: String -> Char -> Lexeme -> Lexeme -> (Lexeme, String)
scanComp "" _ s _ = (s,"")
scanComp (x:xs) f s c | x == f = (c,xs)
		      | otherwise = (s,x:xs)

scan2 :: String -> Char -> Lexeme -> (Lexeme, String)
scan2 "" _ _ = error "scan2: EOF"
scan2 (x:xs) f l | x == f = (l,xs)
		 | otherwise = error "scan2: wrong char"


isIdChar :: Char -> Bool
isIdChar '_' = True
isIdChar '?' = True
isIdChar c   = isAlpha c || isDigit c

scanIdentOrReserved :: Char -> String -> (Lexeme, String)
scanIdentOrReserved c s =
   let (cs,s') = span isIdChar s
       name = c:cs
       reserved = ["if", "in", "let", "letrec",
		   "else", "then", "case", "of"]
   in
      if name `elem` reserved then
         (L_res name, s')
      else
         (L_id name, s')

escapeCode :: Char -> Char
escapeCode 'n'  = '\n'
escapeCode '\'' = '\''
escapeCode '\\' = '\\'
escapeCode '"'  = '"'
escapeCode _    = error "escape code"


scanString :: String -> (String, String)
scanString "" = error "unterminated string"
scanString (c:cs) =
   case c of
      '"'  -> ("", cs)
      '\\' -> let (r,cs') = scanString (tail cs)
	      in (escapeCode (head cs) : r, cs')
      _ -> if isPrint c then
              let (r,cs') = scanString cs
              in (c:r, cs')
           else
	      error "unprintable character in string"


scanChar :: String -> (Lexeme, String)
scanChar ('\\':c:'\'':cs) = (L_char (escapeCode c), cs)
scanChar (c:'\'':cs) = (L_char c, cs)
scanChar _ = error "character literal"

scanNumber :: Char -> String -> (Lexeme, String)
scanNumber d1 s = let (ds,s') = span isDigit s in
		  case s' of
                    ('.' : s'') -> case scanNumber (head s'') (tail s'') of
				     (L_num n2, s3) -> (L_num (d1 : ds ++ "." ++ n2), s3)
                    _ -> (L_num (d1:ds), s')
