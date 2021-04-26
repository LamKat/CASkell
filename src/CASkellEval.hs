module CASkellEval where 

import Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Extra
import Data.Functor
import Poly 
import CASkellParser


type Env = Map String Polynomial 

evalProg :: Prog -> [String]
evalProg (P stmts) = evalState (mapMaybeM evalStmt stmts) M.empty 

evalStmt :: Stmt -> State Env (Maybe String)
evalStmt (Print v) = Just . show <$> evalExpr v 
evalStmt (Assign n v) = do 
        e <- evalExpr v 
        modify (M.insert n e)
        return Nothing  



-- data Expr = Var     String 
--           | Const   Polynomial 
--           | Pow     Expr Integer
--           | Plus    Expr Expr 
--           | Times   Expr Expr 
--           | Neg     Expr 
--           | GCD     Expr Expr 
--           | Norm    Expr 

evalExpr :: Expr -> State Env Polynomial
-- obv this is use of an escape hatch is bad, but I'm not implementing a static type checker. 
evalExpr (Var v) = M.findWithDefault (error $ "Unable to find variable " ++ v) v <$> get
evalExpr (Const p) = return p 
evalExpr (Pow e i) = flip pow i <$> evalExpr e 
evalExpr (Plus e1 e2) = add <$> evalExpr e1 <*> evalExpr e2 
evalExpr (Times e1 e2) = multiply <$> evalExpr e1 <*> evalExpr e2 
evalExpr (Div e1 e2) = div_ <$> evalExpr e1 <*> evalExpr e2
evalExpr (Mod e1 e2) = modulo <$> evalExpr e1 <*> evalExpr e2
evalExpr (Neg e) = neg <$> evalExpr e 
evalExpr (Norm e) = canonicalise <$> evalExpr e
evalExpr (GCD e1 e2) = gcd_ <$> evalExpr e1 <*> evalExpr e2

    