-- http://davidchristiansen.dk/tutorials/implementing-types-hs.pdf
-- https://www.irccloud.com/pastebin/raw/dljokfbX/nbe.hs
{-# language ConstraintKinds #-}
-- type MonadFail = Monad

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving Show

type Env a = [(Name,a)]

data HpVal
  = HVClosure (Env HpVal) Name Expr | HVNeutral Neutral
  deriving Show

data Neutral -- stuff that cannot be reduced yet
  = NVar Name | NApp Neutral HpVal
  deriving Show

fresh :: [Name] -> Name -> Name
fresh xs x
  | elem x xs = fresh xs (x ++ "'")
  | otherwise = x

extend :: Name -> a -> Env a -> Env a; extend x v xs = (x,v):xs

evalE :: MonadFail m => Env HpVal -> Expr -> m HpVal
evalE e (Var x) = case lookup x e of
  Just v -> pure v
  Nothing -> fail "bad var"
evalE e (App f x) = do
  fv <- evalE e f
  xv <- evalE e x
  apV fv xv
evalE e (Lam x b) = pure $ HVClosure e x b

apV :: MonadFail m => HpVal -> HpVal -> m HpVal
apV (HVClosure e x b) v = evalE (extend x v e) b
apV (HVNeutral n) v = pure $ HVNeutral (NApp n v)

v2e :: MonadFail m => [Name] -> HpVal -> m Expr
v2e xs (HVClosure e x b) = do
  let x' = fresh xs x
  bv <- evalE (extend x (HVNeutral $ NVar x') e) b
  Lam x' <$> v2e (x':xs) bv
v2e xs (HVNeutral n) = go n where
  go (NVar x) = pure $ Var x
  go (NApp n v) = App <$> go n <*> v2e xs v

program :: MonadFail m => Env Expr -> m (Env HpVal)
program = go [] where
  go e [] = pure e
  go e ((x,t):xs) = do
    v <- evalE e t
    go (extend x v e) xs

nf :: MonadFail m => Env HpVal -> Expr -> m Expr
nf e t = do
  v <- evalE e t
  v2e [] v

example :: MonadFail m => m Expr
example = do
  e <- program
    [ ("id",Lam "x" $ Var "x")
    , ("const", Lam "x" $ Lam "y" $ Var "x")
    ]
  nf e $ App (Var "const") (Var "id")

main :: IO ()
main = example >>= print
