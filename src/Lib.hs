{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal (op x y)
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal (op x y)
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of
    Just x -> x
    Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env
    | op == "/" && eval e2 env == IntVal 0 = ExnVal "Division by 0"
    | otherwise = liftIntOp op' v1 v2
    where
        Just op' = H.lookup op intOps
        v1 = eval e1 env
        v2 = eval e2 env

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = liftBoolOp op' v1 v2
    where
        Just op' = H.lookup op boolOps
        v1 = eval e1 env
        v2 = eval e2 env

eval (CompOpExp op e1 e2) env = liftCompOp op' v1 v2
    where
        Just op' = H.lookup op compOps
        v1 = eval e1 env
        v2 = eval e2 env

--- ### If Expressions

eval (IfExp e1 e2 e3) env = case eval e1 env of
    BoolVal True -> eval e2 env
    BoolVal False -> eval e3 env
    _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = case eval e1 env of
    CloVal params body env' -> eval body (aux (zip params [eval a env | a <- args]) env')
        where
            aux [] env'' = env''
            aux ((k, v) : xs) env'' = aux xs (H.insert k v env'')
    _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = eval body env'
    where
        env' = aux [(str, eval exp env) | (str, exp) <- pairs] env
        aux [] env = env
        aux ((k, v) : pairs) env = aux pairs (H.insert k v env)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, env')
    where
        env' = H.insert var (eval e env) env

--- ### Sequencing

exec (SeqStmt stmts) penv env = aux stmts penv env ""
    where
        aux [] penv' env' str = (str, penv', env')
        aux (s : ss) penv' env' str =
            let
                (str', penv'', env'') = exec s penv' env'
            in
                aux ss penv'' env'' (str ++ str')

--- ### If Statements

exec (IfStmt exp s1 s2) penv env = case eval exp env of
    BoolVal True -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = case H.lookup name penv of
    Just (ProcedureStmt _ params body) -> exec body penv (aux (zip params [eval a env | a <- args]) env)
        where
            aux [] env' = env'
            aux ((k, v) : xs) env' = aux xs (H.insert k v env')
    Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
