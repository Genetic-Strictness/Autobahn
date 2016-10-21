data Script = Script [Method]

data Block = Block [Stmt]

data Stmt = If Expr Block Expr Block Block
            |While Expr Block
            

data Type =   TyInt
            | TyObject

data Expr =   Binop Bop Expr Expr
            | Unop Uop Expr
            | Id Ident
            | TypeDecl Ident Type [Expr]
            | Const
            | MethodCall Ident Expr [Expr]
            | Alloc Expr Expr [Expr]

data Uop =  Not
            |Neg


data Ident = Ident String

data Method = Method Type String [(Type, String)]


data Bop = Plus | Minus | Mult | Div | Mod | And | Or | Eq | Neq
    
data LValue =   Assign Ident Expr
              | Dot Ident Type [Expr]



instance Show Bop where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or = "||"
  show Eq = "=="
  show Neq = "!="

instance Show Uop where
  show Not = "!"
  show Neg = "-"

instance Show Type where
  show TyInt = "Int"
  show TyObject = "Object"



instance Show Expr where
  show Binop bop expr1 expr2 = show expr1 + " " + show bop + " " show expr2
  show Unop uop expr = show uop + show expr 
