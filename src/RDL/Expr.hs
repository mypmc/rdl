module RDL.Expr where

type Sym = String

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq,Show)

data Expr
  = Abs Sym
        Expr
  | App Expr
        Expr
  | Var Sym
  | Num Int
  | Binop Op
          Expr
          Expr
  deriving (Eq,Show)

parens :: String -> String
parens s = "(" ++ s ++ ")"

sourceOp :: Op -> String
sourceOp Add = " + "
sourceOp Sub = " - "
sourceOp Mul = " * "

source :: Expr -> String
source (Abs x e) = parens $ "\\" ++ x ++ " -> " ++ source e
source (App e1 e2) = parens $ source e1 ++ " " ++ source e2
source (Binop op e1 e2) = parens $ source e1 ++ sourceOp op ++ source e2
source (Var x) = x
source (Num n) = show n

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul
