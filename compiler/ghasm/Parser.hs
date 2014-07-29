module Parser (parseProgram) where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad

import Ghc
import Lang

type Parser a = GenParser Char GhasmState a

data GhasmState = ST { stDefns :: [(String, Arg)],
                       stMacros :: [(String, ([String],[Stmt]))]
                     }

emptyState :: GhasmState
emptyState = ST [] []

-- add new or modify existing assoc
assoc :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
assoc k v [] = [(k,v)]
assoc k v (b@(k1,_) : bs) | k1 == k   = (k,v) : bs
                          | otherwise = b : assoc k v bs

setDefn :: String -> Arg -> GhasmState -> GhasmState
setDefn name value st = st{stDefns = assoc name value (stDefns st)}

setMacro :: String -> ([String],[Stmt]) -> GhasmState -> GhasmState
setMacro name stmts st = st{stMacros = assoc name stmts (stMacros st)}

getDefn :: String -> GhasmState -> Maybe Arg
getDefn name st = lookup name (stDefns st)

getMacro :: String -> GhasmState -> Maybe ([String],[Stmt])
getMacro name st = lookup name (stMacros st)

parseProgram :: String -> String -> [Stmt]
parseProgram fname input =
   case runParser pProgram emptyState fname (fixNewlines input) of
      Left err -> error ("parse error: " ++ show err)
      Right ds -> ds

-- add separator at end of each line
fixNewlines :: String -> String
fixNewlines =  unlines . map fixLine . lines
  where
    fixLine ""        = " :"
    fixLine (';' : s) = " :"
    fixLine (c:s)     = c : fixLine s 


pProgram :: Parser [Stmt]
pProgram =  do whiteSpace
               is <- many1 pTopLevel
               return (concat is)

-- value definition, macro definition, or instructions
pTopLevel :: Parser [Stmt]
pTopLevel =   do { reserved "def"
                 ; name <- identifier
                 ; val  <- pArg
                 ; colon
                 ; updateState (setDefn name val)
                 ; return []
                 }
          <|> do { reserved "macro"
                 ; name <- identifier
                 ; args <- identifier `sepBy` comma
                 ; colon
                 ; body <- pStmts
                 ; reserved "endm"
                 ; colon
                 ; updateState (setMacro name (args,body))
                 ; return []
                 }
          <|> pStmt


pStmts :: Parser [Stmt]
pStmts =  do st <- many1 pStmt
             return (concat st)

pStmt :: Parser [Stmt]
pStmt =   liftM (:[]) pLoop
      <|> do { reserved "if"
             ; e <- pCond
             ; colon
             ; c <- pStmts
             ;     do { reserved "fi" ; colon ; return [SIf e c []] }
               <|> do { reserved "else"; colon
                      ; a <- pStmts; reserved "fi"; colon
                      ; return [SIf e c a]
                      }
             }
      <|> pUnOp "inc" <|> pUnOp "dec"
      <|> pBinOp "mov"
      <|> pBinOp "add" <|> pBinOp "sub" <|> pBinOp "mul" <|> pBinOp "div"
      <|> pBinOp "and" <|> pBinOp "or"  <|> pBinOp "xor"
      <|> do { reserved "sys"; s <- identifier; colon;
               return [makeSysCall s] }
      <|> do { reserved "hlt"; colon; return [SHalt] }
      <|> do { mac <- identifier; args <- pSArg `sepBy` comma; colon
             ; st <- getState
             ; case getMacro mac st of
                 Nothing -> fail ("undefined macro: " ++ mac)
                 Just (xs,body) ->
                   if length xs == length args then 
                     return (macroExpand (zip xs args) body)
                   else
                     fail ("wrong number of parameters to macro: " ++ mac)
             }
      <|> do { colon; return [] }

pLoop :: Parser Stmt
pLoop =  reserved "do" >> (while <|> doWhile)
  where
    while   = do { reserved "while" ; e <- pCond ; colon
                 ; body <- pStmts
                 ; reserved "od" ; colon
                 ; return (SWhile e body)
                 }
    doWhile = do { colon
                 ; body <- pStmts
                 ; reserved "while"
                 ; e <- pCond ; colon
                 ; return (SDoWhile body e)
                 }

pUnOp :: String -> Parser [Stmt]
pUnOp op = do reserved op
              e <- pSArg
              colon
              return [SUnOp op e]

pBinOp :: String -> Parser [Stmt]
pBinOp op = do reserved op
               e1 <- pSArg
               comma
               e2 <- pSArg
               colon
               return [SBinOp op e1 e2]


directArg :: Parser Arg 
directArg =  liftM (Imm . fromIntegral) natural
             <|> liftM Reg identifier

pArg :: Parser Arg
pArg =  directArg <|> liftM indirect (brackets directArg)
  where indirect (Imm n) = Mem n
        indirect (Reg s) = Ind s

pSArg :: Parser Arg
pSArg =  do a <- pArg
            st <- getState
            case a of
              Reg name -> case getDefn name st of
                            Just v -> return v
                            Nothing -> return a
              _ -> return a

pCond :: Parser Cond
pCond =  do { e1 <- pSArg
            ; op <- operator
            ; e2 <- pSArg
            ; return (Cond op e1 e2)
            }

lexer :: P.TokenParser GhasmState
lexer  = P.makeTokenParser
           (emptyDef {
               P.commentLine = ";",
               P.reservedNames = ["if","else","fi","while","do","od",
                                  "def","macro","endm",
                                  "inc","dec",
                                  "mov",
                                  "add","sub","mul","div",
                                  "and","or","xor",
                                  "hlt","sys"],
               P.reservedOpNames = [],
               P.identStart     = letter,
               P.identLetter    = alphaNum <|> char '_',
               P.opStart        = oneOf "=<>/",
               P.opLetter       = char '=',
               P.caseSensitive  = False
               })
         
-- For efficiency, we will bind all the used lexical parsers at toplevel.

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
brackets  = P.brackets lexer
braces    = P.braces lexer
semi      = P.semi lexer
comma     = P.comma lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
operator  = P.operator lexer
colon     = P.colon lexer


makeSysCall :: String -> Stmt
makeSysCall s = case s of
                  "move" -> SInt 0
                  "lambda1" -> SInt 1
                  "lambda2" -> SInt 2
                  "index" -> SInt 3
                  "startpos" -> SInt 4
                  "curpos" -> SInt 5
                  "status" -> SInt 6
                  "map" -> SInt 7
                  "trace" -> SInt 8
                  _ -> error ("unknown system call: " ++ s)

macroExpand :: [(String,Arg)] -> [Stmt] -> [Stmt]
macroExpand bindings body = map (expandArgs bindings) body


