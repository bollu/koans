-- https://www.cs.ox.ac.uk/projects/utgp/school/conor.pdf
-- Last chapter
-- import Data.Nat
-- open import Data.Nat (ℕ; zero; suc; _+_; _*_; _^_; _∸_)
open import Function
open import Data.Nat
open import Data.Fin
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

--  Agda.Builtin.Nat


sum prod : (n : ℕ) → (Fin n → ℕ) → ℕ
sum zero _ = 0
sum (suc n) f = (f zero) Data.Nat.+ (sum n (f ∘ suc))

prod zero _ = 1
prod (suc n) f = (f zero) Data.Nat.* (prod n (f ∘ suc))

-- | define functions that are used during
-- dependent "value" creation
mutual 
  data FTy : Set where
    fin : ℕ → FTy
    σ : (S : FTy) (T : Fin (size S) → FTy) → FTy
    π : (S : FTy) (T : Fin (size S) → FTy) → FTy
    
  size : FTy → ℕ
  size (fin n) = n
  size (σ S T) = sum (size S) λ s → size (T s)
  size (π S T) = prod (size S) λ s → size (T s)
      

-- | fog for forget
fog : forall {n } → Fin n → ℕ
fog zero = zero
fog (suc i) = suc (fog i)




_ = begin
    size (σ (fin 101 ) (λ s → fin (fog s)))
    ≡ 5050 ∎
  
