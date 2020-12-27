{-# LANGUAGE MultiParamTypeClasses #-}

-- A vector space V over a field F, is a 
-- set V and a set F
-- such that:
-- 1. F is a field
--     
--
-- 2. (V, +, 0) is an abelian group
--     forall v. v + 0 = 0 + v = v
--     forall v, exists w, v + w = 0
--     forall v w, v + w = w + v
--
--3. scaling: F x V -> V
--
--
--
-- A group G is a set G, equipped with a binary operator
-- *: G x G -> G , and an element e ∈ G such that:
-- assoative: forall g1, g2, g3, (g1 * g2) * g3 = g1 * (g2 * g3)
-- identity: forall g, g * e = e
-- inverse: forall g, g * g^{-1} = e
--
-- Algebraic structures : Math :: Typeclass : Hakell











































class AbelianGroup a where
  gop :: a -> a -> a
  ginv :: a -> a
  gid :: a

-- | A field over a set f has the following operations:
class Field f where
  fadd :: f -> f -> f -- fadd : f -> f -> f
  fmul :: f -> f -> f -- fmul :: f -> f -> f
  fzero :: f -- fzero :: f
  fone :: f
  faddinv :: f -> f -- 
  fmulinv :: f -> f


class (AbelianGroup v, Field f) => 
  VectorSpace v f  where
      vscale :: f -> v -> v

class VectorSpace v f => InnerProductSpace v f where
   innerProduct :: v -> v -> f


gramSchmidt :: InnerProductSpace v f => [v] -> [v]
