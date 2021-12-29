{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Initial
data Exp = Lit Int | Neg Exp | Add Exp Exp

ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = -1 *  eval e
eval (Add e1 e2) = eval e1 + eval e2

type Repr = Int
lit :: Int -> Repr
lit n = n
neg :: Repr -> Repr
neg e = - e
add :: Repr -> Repr -> Repr
add e1 e2 = e1 + e2

view:: Exp -> String
view (Lit n) = show n
view (Neg e) = "(-" ++ view e ++ ")"
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"


-- | Symantics
class ExpSYM repr where
  lit' :: Int -> repr
  neg' :: repr -> repr
  add' :: repr -> repr -> repr


tf1' :: ExpSYM repr => repr
tf1' = add' (lit' 8) (neg' (add' (lit' 1) (lit' 2)))


instance ExpSYM Int where
  lit' n = n
  neg' e = -1 * e
  add' e1 e2 = e1 + e2


instance ExpSYM String where
  lit' n = show n
  neg' e = "(-" ++ e ++ ")"
  add' e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"


class MulSYM repr where
  mul' :: repr -> repr -> repr

instance MulSYM Int where
  mul' e1 e2 = e1 * e2

instance MulSYM String where
  mul' e1 e2 = "(" ++ e1 ++ "*" ++ e2 ++ ")"

tf2' :: (MulSYM repr, ExpSYM repr) => repr
tf2' = mul' (lit' 10) (add' (lit' 8) (neg' (add' (lit' 1) (lit' 2))))


data Tree = Leaf String | Node String [Tree] deriving (Eq, Read, Show)


instance ExpSYM Tree where
  lit' n = Node "Lit" [Leaf $ show n]
  neg' e = Node "Neg" [e]
  add' e1 e2 = Node "Add" [e1,e2]

type ErrMsg = String
safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
  [( x, "")] -> Right x
  _ -> Left $ "Read error: " ++ s


instance (ExpSYM repr, ExpSYM repr') => ExpSYM (repr,repr') where
  lit' x = (lit' x, lit' x)
  neg' (e1, e2) = (neg' e1, neg' e2)
  add' (e11,e12) (e21, e22) = (add' e11 e21, add' e12 e22)
