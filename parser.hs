{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Data.List
import Data.Monoid


 -- Haskell encoding of parser combinators

data Parser a = Parser { parse :: String -> [(a, String)] }

(@>) :: String -> Parser a -> [(a, String)]
(@>) s p = (parse p) s 

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser {
    parse = \s -> fmap (\(a, s) -> (f a, s)) (s @> p)
  }

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser {
    parse = \s -> [(a, s)]
  }

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = Parser {
    parse = \s -> do 
                    (f, s') <- s @> pf
                    (a, s'') <- s' @> pa
                    return (f a, s'')
  }

-- Fun fact: once you have Monad, you can simply write Applicative
-- trivially
{-
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser {
    parse = \s -> [(a, s)]
  }

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = do
         f <- pf
         a <- pa
         return $ f a
-}

instance Monad Parser where
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa a_pb = Parser {
    parse = \s -> do 
            (a, sa) <- (s @> pa)
            (b, sb) <- sa @> (a_pb a)
            return (b, sb)
  }

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) pa pa' = Parser $
      (\s -> let out = s @> pa in 
        if null out  then s @> pa' else out)

  -- ?? v is this correct? v
  empty :: Parser a
  empty = Parser (const [])
  
eatc :: Char -> (Char -> a) -> Parser a
eatc c f = Parser $ \s -> 
  case s of 
    [] -> []
    (c':cs) -> if c == c' then [(f c, cs)] else []

eats_ :: [Char] -> [Char] -> ([Char] -> a) -> Parser a
eats_ accum [] f = return (f accum)
eats_ accum (c:cs) f = eatc c id >>= \c -> eats_ (accum ++ [c]) cs f
 
eats :: [Char] -> ([Char] -> a) -> Parser a
eats s f = eats_ [] s f

starof :: Parser a -> Parser [a]
starof p = (p >>= \a -> starof p >>= \as -> return (a:as)) <|> (fmap (:[]) p) 
    
p :: Parser Int
p = eatc 'p' (const 1)
q :: Parser Int
q = eatc 'q' (const 2)

pwhitespace :: Parser [Char]
pwhitespace = (starof (eatc ' ' id <|> eatc '\t' id <|> eatc '\n' id)) <|> (Parser (\s -> [("", s)]))

pdigit :: Parser Char
pdigit = Parser (\s -> case s of 
                        [] -> []
                        (c:cs) -> if c `elem` ['0'..'9'] then [(c, cs)] else [])

pint :: Parser Int
pint = read <$> (starof pdigit)

-- Applicative Parsing
-- B = '(' B ')' | 'a' <- grammar is not regular, but is context free
data Brackets = Brackets Brackets | Done



instance (Show Brackets) where
  show (Brackets b) = "[" ++ (show b) ++ "]"
  show (Done) = "a"

pbracket :: Parser Brackets
pbracket = eatc 'a' (const Done) <|> 
         (Brackets <$>  ((eatc '[' id) *> (pbracket <* (eatc ']' id))))

-- Monadic parsing using the Monad typeclass
-- Needed for context sensitivity
data Expr = Add Term Expr | Sub Term Expr | LoneExpr Term
data Term = Mult Factor Term | Div Factor Term | LoneTerm Factor
data Factor = Base Int | Bracketed Expr | Negated Factor

instance Show Factor where
  show :: Factor -> String
  show (Base i) = show i
  show (Bracketed e) = show e
  show (Negated e) = "-" <> show e

instance Show Term where
  show :: Term -> String
  show (Mult f t) = "(" <> (show f) <>  " * " <> (show t) <> ")"
  show (Div f t) = "(" <> (show f) <> " / " <> (show t) <> ")"
  show (LoneTerm f) = (show f)

instance Show Expr where
  show :: Expr -> String
  show (Add t e) = (show t) <> " + " <> (show e)
  show (Sub t e) = (show t) <> " - " <> (show e) 
  show (LoneExpr t) = (show t)


pfactor :: Parser Factor
pfactor = pwhitespace >> 
  ((Base <$> pint) <|> (do
            eatc '(' id
            expr <- pexpr 
            eatc ')' id
            return $ Bracketed expr)
  <|>
  (do
    eatc '-' id
    factor <- pfactor
    return $ Negated factor))
    

pterm :: Parser Term
pterm = pwhitespace >> 
  ((do
    f1 <- pfactor
    pwhitespace
    op <- eatc '*' id <|> eatc '/' id
    pwhitespace
    t1 <- pterm
    return $ if op == '*' then Mult f1 t1 else Div f1 t1)
  <|> LoneTerm <$> pfactor)


pexpr :: Parser Expr
pexpr = pwhitespace >> 
  ((do
    t1 <- pterm
    pwhitespace
    op <- eatc '+' id <|> eatc '-' id
    pwhitespace
    t2 <- pexpr
    return $ if op == '+' then Add t1 t2 else Sub t1 t2)
  <|> LoneExpr <$> pterm)

evalExpr :: Expr -> Float
evalExpr (Add t e) = evalTerm t + evalExpr e
evalExpr (Sub t e) = evalTerm t - evalExpr e
evalExpr (LoneExpr t) = evalTerm t

evalTerm :: Term -> Float
evalTerm (Mult f t) = evalFactor f * evalTerm t
evalTerm (Div f t) = evalFactor f / evalTerm t
evalTerm (LoneTerm f) = evalFactor f

evalFactor :: Factor -> Float
evalFactor (Base i) = fromIntegral i
evalFactor (Bracketed e) = evalExpr e
evalFactor (Negated f) = -1.0 * (evalFactor f)


parseAndEval :: String -> Maybe Float
parseAndEval str = case parsed of
                     (x:xs) -> Just (evalExpr (fst x))
                     [] -> Nothing
                   where
                     parsed = str @> pexpr

