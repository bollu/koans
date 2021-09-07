{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.Map as M
import Control.Arrow
import Data.Monoid

-- | relation with finite support.
newtype Relation v r = Relation (M.Map v r) deriving(Eq, Show)

class Eq r => Semiring r where
    plus :: r -> r -> r
    times :: r -> r -> r
    zero :: r
    one :: r

-- | protected constructor
relation :: (Ord v, Semiring r) => (M.Map v r) -> Relation v r
relation f = Relation $ M.filter (/= zero) f

emptyRel :: (Ord v, Semiring r) => Relation v r; emptyRel = relation mempty

unionRel :: (Ord v, Semiring r) => Relation v r -> Relation v r -> Relation v r
unionRel (Relation f) (Relation g) = relation $ M.unionWith plus f g


-- | TODO: add fold syntax to haskell, lol?
projectRel :: (Ord v, Semiring r, Ord w) => (v -> w) -> Relation v r -> Relation w r
projectRel p (Relation f) = relation $ foldl (\m (w, r) -> M.insertWith plus w r m) mempty . map (p *** id) . M.toList  $ f


selectRel :: (Ord v, Semiring r) => (v -> Bool) -> Relation v r -> Relation v r
selectRel p (Relation f) = relation $ M.filterWithKey (\v r -> p v) f

natjoinRel :: (Ord v, Semiring r) => Relation v r -> Relation v r -> Relation v r
natjoinRel (Relation f) (Relation g) = relation $ M.intersectionWith times f g

mapRel :: (Ord v, Semiring r, Semiring s) => (r -> s) -> Relation v r -> Relation v s
mapRel phi (Relation f) = relation $ M.map phi f

instance Semiring Bool where
    plus = (||)
    times = (&&)
    zero = False
    one = True

instance (Eq a, Num a) => Semiring a where
    plus = (+)
    times = (*)
    zero = 0
    one = 1


newtype Fuzzy = Fuzzy Int deriving (Eq)-- integers between [0, 1]
instance Semiring Fuzzy where
   plus (Fuzzy n) (Fuzzy m) = Fuzzy (max n m)
   times (Fuzzy n) (Fuzzy m) = Fuzzy (min n m)
   zero = Fuzzy 0
   one = Fuzzy 1


-- | a generating function, really...
type Poly r = Relation Int r

-- | can be generalized, probably with dirichlet convolution.
instance (Semiring r) => Semiring (Poly r) where
    zero = emptyRel
    one = relation $ M.singleton 0 one
    plus (Relation p) (Relation q)  = relation $ M.unionWith plus p q
    times (Relation p) (Relation q) = 
      relation . M.fromList $ [(kp + kq, vp `times` vq) | (kp, vp) <- (M.toList p), (kq, vq) <- (M.toList q) ]


-- Next: power series via lazy lists.
