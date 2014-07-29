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
            ; symbol "="
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
         do { symbol "\\"
            ; ns <- many1 identifier
            ; symbol "->"
            ; e <- pExpr
            ; return (mkLambda ns e)
            }
         <|> pExpr0
         <?> "expression"

pExpr0   :: Parser Expr
pExpr0    = buildExpressionParser table pApp
        <?> "expr0"

table   = [[op "*" "*" AssocLeft, op "/" "/" AssocLeft]
          ,[op "+" "+" AssocLeft, op "-" "-" AssocLeft]
          ,[op ":" "cons" AssocRight]
          ,[op "==" "=" AssocNone, op ">" ">" AssocNone, op ">=" ">=" AssocNone]
          ,[op "&&" "&&" AssocRight]
          ,[op "||" "||" AssocRight]
          ]          
        where
          op s f assoc
             = Infix (do{ symbol s; return (\a b -> mkApp [CFun f,a,b]) }) assoc

pApp :: Parser Expr
pApp =  do { es <- many1 aExpr; return (mkApp es) }
        <?> "application"



lexer :: P.TokenParser ()
lexer  = P.makeTokenParser haskellDef
         
-- For efficiency, we will bind all the used lexical parsers at toplevel.

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
brackets  = P.brackets lexer
semi      = P.semi lexer
comma     = P.comma lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

aExpr  :: Parser Expr
aExpr  =    parens pExpr
        <|> do { es <- brackets (sepBy pExpr comma); return (mkList es) }
        <|> do { n <- natural; return (CNum (show n)) }
        <|> do { x <- identifier; return (EVar x) }
        <?> "aexpr"


mkList :: [Expr] -> Expr
mkList [] = CNil
mkList (x:xs) = mkApp2 (CFun "cons") x (mkList xs)

mkApp2 :: Expr -> Expr -> Expr -> Expr
mkApp2 f a b = EApp (EApp f a) b

mkLambda :: [String] -> Expr -> Expr
mkLambda [] e = e
mkLambda (x:xs) e = ELam x (mkLambda xs e)
