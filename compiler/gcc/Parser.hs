module Parser (parseTL) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P

import Expr

parseTL :: String -> [Defn]
parseTL input = case parse pTopLevel "" input of
                  Left err -> error ("parse error: " ++ show err)
                  Right ds -> ds

pTopLevel :: Parser [Defn]
pTopLevel = do whiteSpace
               ds <- pDefn `sepEndBy1` semi
               return ds

pDefn :: Parser Defn
pDefn =  do { (x:xs) <- many1 identifier
            ; _ <- symbol "="
            ; e <- pExpr
            ; return (x, mkLambda xs e)
            }

pDefns :: Parser [Defn]
pDefns =  sepBy1 pDefn semi

pExpr :: Parser Expr
pExpr =  do { reserved "if"
            ; e <- pExpr
            ; reserved "then"
            ; c <- pExpr
            ; reserved "else"
            ; a <- pExpr
            ; return (EIf e c a)
            }
         <|>
         do { reserved "let"
            ; ds <- pDefns
            ; reserved "in"
            ; e <- pExpr
            ; return (ELet ds e)
            }
         <|>
         do { _ <- symbol "\\"
            ; ns <- many1 identifier
            ; _ <- symbol "->"
            ; e <- pExpr
            ; return (mkLambda ns e)
            }
         <|> pExpr0
         <?> "expression"

pExpr0   :: Parser Expr
pExpr0    = buildExpressionParser table pApp
        <?> "expr0"

table :: [[Operator Char () Expr]]
table   = [[op "*" "*" AssocLeft, op "/" "/" AssocLeft]
          ,[op "+" "+" AssocLeft, op "-" "-" AssocLeft]
          ,[op ":" "cons" AssocRight]
          ,[op "==" "=" AssocNone, op ">" ">" AssocNone, op ">=" ">=" AssocNone]
          ,[op "&&" "&&" AssocRight]
          ,[op "||" "||" AssocRight]
          ]          
        where
          op s f assoc
             = Infix (do{ _ <- symbol s; return (\a b -> mkApp [CFun f,a,b]) }) assoc

pApp :: Parser Expr
pApp =  do { es <- many1 aExpr; return (mkApp es) }
        <?> "application"



lexer :: P.TokenParser ()
lexer  = P.makeTokenParser haskellDef
         
-- For efficiency, we will bind all the used lexical parsers at toplevel.

whiteSpace :: Parser ()
reserved, symbol   :: String -> Parser ()
identifier :: Parser String
comma, semi :: Parser ()
brackets :: Parser a -> Parser a
natural :: Parser Integer

whiteSpace= P.whiteSpace lexer
symbol    = discard . P.symbol lexer
natural   = P.natural lexer
brackets  = P.brackets lexer
semi      = discard $ P.semi lexer
comma     = discard $ P.comma lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer

discard :: Parser a -> Parser ()
discard p = p >> return ()


aExpr  :: Parser Expr
aExpr  =    pOrTuple
        <|> do { es <- brackets (sepBy pExpr comma); return (mkList es) }
        <|> do { n <- natural; return (CNum (show n)) }
        <|> do { x <- identifier; return (EVar x) }
        <?> "aexpr"

pOrTuple :: Parser Expr
pOrTuple =  do _  <- symbol "("
               es <- pExpr `sepBy` comma
               _  <- symbol ")"
               case es of
                 []  -> fail "unit () not supported"
                 [e] -> return e
                 xs  -> return $ CTuple xs


mkList :: [Expr] -> Expr
mkList [] = CNil
mkList (x:xs) = CPair x (mkList xs)

mkLambda :: [String] -> Expr -> Expr
mkLambda [] e = e
mkLambda (x:xs) e = ELam x (mkLambda xs e)
