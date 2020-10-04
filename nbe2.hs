type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving Show

type Env a = [(Name,a)]

nfnaive :: Expr -> Env Expr -> Maybe Expr
nfnaive (Var name) env = lookup name env
nfnaive (App fe xe) = do
   -- need to implement substitution.
   f <- nfnaive fe env
   x <- nfnaive xe env 
   case f of
    Lam fx frhs -> 
    _ -> pure (App f x) -- can't apply. ERROR

