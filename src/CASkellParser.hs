module CASkellParser where

import Poly

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>),many,(<?>))
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Data.Functor
import GHC.Integer
import Data.Ratio


-- AST --

data Prog = P [Stmt]
            deriving (Show)
                     
data Stmt = Assign String Expr 
          | Print Expr 
            deriving (Show)

data Expr = Var     String 
          | Const   Polynomial 
          | Pow     Expr Integer
          | Plus    Expr Expr 
          | Times   Expr Expr 
          | Div     Expr Expr 
          | Mod     Expr Expr 
          | Neg     Expr 
          | GCD     Expr Expr 
          | Norm    Expr 
            deriving (Show)

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
                                     , "normalise"
                                     , "x" ]
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
        <|>  Print <$> (reserved "print" *> parens expr_1_Parser) 

expr_1_Parser :: P.Parser Expr
expr_1_Parser = P.try (Pow <$> expr_2_Parser <*> exponentParser)
        <|> P.chainl1 expr_2_Parser opParser
        -- <|> expr_2_Parser

exponentParser :: P.Parser Integer 
exponentParser = symbol "^" *> natural 
        --     <|> P.choice [symbol "¹" $> 1]--

expr_2_Parser :: P.Parser Expr
expr_2_Parser = Var <$> identifier
        --     <|> P.chainl1 expr_1_Parser opParser 
            <|> Const <$> parens polyParse 
            <|>  reserved "gcd" *>
                    parens (do {
                        lhs <- expr_1_Parser ;
                        symbol "," ; 
                        rhs <- expr_1_Parser ;
                        return $ GCD lhs rhs
                    })
            <|> Norm <$> (reserved "normalise" *> parens expr_1_Parser)
        --     <|> parens expr_1_Parser


opParser :: P.Parser (Expr -> Expr -> Expr) 
opParser = symbol "*" $> Times
        <|> symbol "/" $> Div
        <|> symbol "%" $> Mod

-- (x^2 - x + 4)


-- This is a nasty hack. 
polyParse :: P.Parser Polynomial
polyParse = do {
                hd <-  (P.option id (symbol "-" $> negateInteger)) >>= termParse ;
                tl <- P.many ((P.choice [
                                symbol "+" $> id ,
                                symbol "-" $> negateInteger
                        ]) >>= termParse) ;
                return $ PolySum  (hd : tl)
        }
        where
                termParse :: (Integer -> Integer) -> P.Parser (Coeff, Exponent)
                termParse symb = do {
                                coeff <- P.option 1 integer ;
                                exp <- P.option 0 (reserved "x" *> P.option 1 exponentParser) ; 
                                return ((symb coeff) % 1,  exp)
                        }


parseFromFile p fname = do { 
        input <- readFile fname
        ; return (P.runParser p () fname input)
       }