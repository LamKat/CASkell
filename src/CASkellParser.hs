module CASkellParser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>),many,(<?>))
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr


-- AST --

data Prog = P [Stmt]
            deriving (Eq, Show)
                     
data Stmt = Assign String Expr 
          | Print Expr 
            deriving (Eq, Show)

data Expr = Var     String 
          | Const   Polynomial 
          | Pow     Expr Integer
          | Plus    Expr Expr 
          | Times   Expr Expr 
          | Neg     Expr 
          | GCD     Expr Expr 
          | Norm    Expr 

--
-- x := Poly(x^2 - x + 4)
-- y := Poly(x - 1)
-- z := x × y
-- z := 

-- Prog ::= [Stmt]
-- Stmt ::= var ":=" Expr 
--          "print" Expr 
-- Expr₁ ::= Expr₂ ^ ℕ
--           Expr₂
-- Expr ::= var 
--          Poly
--          Expr Op Expr 
--          GCD(Expr, Expr)
--          Normalise(Expr)
--          (Expr₁)
-- Poly ::= ℕx^ℕ (+ ℕx^ℕ)*
-- Op ::= + | ×



-- Lexer -- 
lexer :: T.TokenParser ()

ldef = emptyDef {  T.identStart = P.letter
                 , T.identLetter = (P.alphaNum <|> P.char '_' <|> P.char '.' 
                                               <|> P.char '$'<|> P.char '#')
                 , T.reservedNames = [ "print"
                                     , "gcd"
                                     , "normalise" ]
                 , T.commentLine = "--"
                 }
 
lexer = T.makeTokenParser ldef

whiteSpace = T.whiteSpace lexer
reserved   = T.reserved lexer
parens     = T.parens lexer
identifier = T.identifier lexer
natural    = T.natural lexer
integer    = T.integer lexer
semi       = T.semi lexer
symbol     = T.symbol lexer

-- Parser --

progParser :: P.Parser Prog
progParser = P <$> P.sepBy1 stmtParser semi 

stmtParser :: P.Parser Stmt
stmtParser = Assign <$> identifier <* symbol ":=" <*> expr_1_Parser 
        <|>  Print <$> expr_1_Parser 

expr_1_Parser :: P.Parser Stmt
expr_1_Parser = Pow <$> expr_2_Parser <* symbol "^" <*> natural
            <|> Pow <$> expr_2_Parser <*> exponentParser

exponentParser :: P.Parser Integer 
exponentParser = symbol "^" <*> natural 
            <|> choice [symbol "¹" $> 1]--

expr_2_Parser :: P.Parser Stmt
expr_2_Parser = Var <$> identifier
            <|> Const <$> parens polyParse 
            <|> chainl1 expr_1_Parser opParser 
            <|> uncurry GCD <$> reserved "gcd" *>
                    parens (do {
                        lhs <- expr_1_Parser ;
                        symbol "," ; 
                        rhs <- expr_1_Parser ;
                        return (lhs, rhs)
                    })
            <|> Norm <$> reserved "normalise" *> parens expr_1_Parser
            <|> parens expr_1_Parser


opParser :: P.Parser BinOp 
opParser = symbol "+" $> Plus
        <|> oneOf "×*" $> Times

polyParse 