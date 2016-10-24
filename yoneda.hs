{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

-- Encoding the Yoneda lemma in Haskell

-- g ~= Nat(Hom(X, -), g)
type Yoneda g x = (forall y. (x -> y) -> g y)

lemma_toyoneda :: Functor g => g x -> Yoneda g x
lemma_toyoneda  g_x yoneda = fmap yoneda  g_x


lemma_fromyoneda :: Functor g => Yoneda g x -> g x
lemma_fromyoneda yoneda = yoneda id

