{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import GHC.Prim
import GHC.Show (show, Show)
import GHC.Base (String, otherwise, isTrue#, 
                 Char(C#), IO, (++), (<>), (==), Bool(..), Eq)
import Test.QuickCheck
-- import GHC.Int -- I#

-- value :: Type

data Day where
  Monday :: Day -- introduction
  Tuesday :: Day
  Wednesday :: Day                                 
  Thursday :: Day
  Friday :: Day
  Saturday :: Day
  Sunday :: Day                                          

showDay :: Day -> String
showDay d = 
  case d of
    Monday -> "Mon"
    Tuesday -> "Tue"
    Wednesday -> "Wed"
    Thursday -> "Thurs"
    Friday -> "Fri"






-- | Z/2Z
data Z2 where
  One :: Z2 -- introduce
  Zero :: Z2 -- introduce

showz2 :: Z2 -> String
showz2 n = 
  case n of
    One -> "one"
    Zero -> "zero"
















-- let incr = plus 1

-- incr 3
-- (plus 1) 3
-- 4
--
--
-- incr x
-- (plus 1) x
-- x + 1


-- Currying / Partial application
-- int xor(int x, int y)
-- xor :: Z2 -> (Z2 -> Z2)
-- xor x y

-- (0 + 1) % 2 === xor
xor :: Z2 -> Z2 -> Z2
xor x y =
  case x of
    Zero -> case y of
                Zero -> Zero
                One -> One
    One -> case y of
            Zero -> One
            One -> Zero

identZ2 :: Z2 -> Z2
identZ2 = xor Zero

notZ2 :: Z2 -> Z2
notZ2 = xor Zero

data Void where




















-- | Curry Howard Correspondence
-- | False -> anything
-- a => b truth table: !a || b
-- false => b: !false || b  = true || b = true
explosion :: Void -> Char
explosion v = case v of 

 
-- A monoid is a set S, along with a binary operator * which is
-- assocative and has an identity element
class Monoid a where
  identity :: a
  mop :: a -> a -> a


-- forall a in S, identity * a = a
prop_monoid_identity_law :: 
  forall a. (Eq a, Monoid a) => a -> Bool
prop_monoid_identity_law aval = 
  mop identity aval == aval

prop_monoid_assoc_law :: forall a. (Eq a, Monoid a) => a -> a -> a -> Bool
prop_monoid_assoc_law av1 av2 av3 = 
  mop av1 (mop av2 av3) == mop (mop av1 av2) av3


instance Monoid String where
  identity = ""
  mop = (++)




-- | synonym for the operation `mop`
infixl 6 <+>
(<+>) :: forall x. Monoid x => x -> x -> x
(<+>) = mop

prop_monoid_assoc_law' :: forall a. (Eq a, Monoid a) => a -> a -> a -> Bool
prop_monoid_assoc_law' av1 av2 av3 = av1 <+> (av2 <+> av3) == (av1 <+> av2) <+> av3



instance Monoid Z2 where
  identity = One
  mop = xor


-- Show quickCheck prop
-- Make quickCheck prop fail
-- show '@'


-- | Implement Show for Z2
instance Show Z2 where
  show b = case b of
                Zero -> "Zero"
                One -> "One"

-- | Implement Eq for Z2
instance Eq Z2 where
  (==) a b  = case a of 
             Zero -> 
                case b of 
                    Zero -> True
                    One -> False;
             One -> case b of 
                        Zero -> False
                        One -> True;


-- https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/Test-QuickCheck.html#t:Gen
instance Arbitrary Z2 where
  arbitrary = elements [Zero,One]

-- | how to incarnate an Int
data MyInt where
  MkMyInt :: Int# -> MyInt

-- | allowed
loopInt :: MyInt
loopInt = loopInt

-- | not allowed
-- loopRaw :: Int#
-- loopRaw = loopRaw

showMyInt :: MyInt -> String -- defined elsewhere

one :: MyInt; one = MkMyInt 1#
two :: MyInt; two = MkMyInt 2#
three :: MyInt; three = MkMyInt 3#


-- | Recursive structures
data ListI where
  EmptyI :: ListI
  ConsI :: MyInt -> ListI -> ListI

li :: ListI
li = ConsI one (ConsI two EmptyI)


showListI :: ListI -> String
showListI xs = case xs of
                    EmptyI -> "EmptyI"
                    -- ConsI :: MyInt -> ListI -> ListI
                    ConsI i xs' -> 
                        "ConsI(" ++ show i ++ ", " ++ showListI xs' ++ ")"
  

data ListC where 
  EmptyC :: ListC
  ConsC :: Char -> ListC -> ListC

lc :: ListC
lc = ConsC 'h' (ConsC 'e' (ConsC 'l' (ConsC 'l' EmptyC)))

showListC :: ListC -> String
showListC xs = case xs of
                    EmptyC -> "EmptyC"
                    ConsC c xs' -> "ConsC(" ++ show c ++ ", " ++ showListC xs' ++ ")"


data List a where 
    EmptyL :: List a
    ConsL :: a -> List a -> List a














lli :: List MyInt
lli = ConsL one (ConsL two EmptyL)

llc :: List Char
llc = ConsL 'h' (ConsL 'e' (ConsL 'l' (ConsL 'l' EmptyL)))




showList :: Show a => List a -> String
showList xs = 
  case xs of
    EmptyL -> "EmptyL"
    -- ConsL :: a -> List a -> List a
    ConsL x xs' -> "ConsL(" ++ show x ++ ", " ++ showList  xs' ++ ")"

instance Show a => Show (List a) where
  show = showList


take :: Int# -> List a -> List a
take i xs = case i of
            0# -> EmptyL
            _ -> case xs of
                    EmptyL -> EmptyL
                    ConsL x xs' -> ConsL x (take (i -# 1#) xs')

-- List processing
id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

unfold :: (a -> a) ->  a -> List a
unfold f a = ConsL a (unfold f (f a))

repeat :: a -> List a
repeat x = unfold id x

ooh  :: List String
ooh = unfold ((++) "o") "h"


loop :: a; loop = loop

index :: Int# -> List a ->  a
index i l = case i of
                0# -> case l of
                        EmptyL -> loop
                        ConsL x _ -> x
                _ -> case l of
                        EmptyL -> loop
                        ConsL x xs -> index (i -# 1#) xs


data Optional a where
  Some :: a -> Optional a
  None :: Optional a


instance (Show a) => Show (Optional a) where
    show (Some x) = "Some(" ++ show x ++ ")"
    show None = "None"

index' :: Int# -> List a ->  Optional a
index' i l = case i of
                0# -> case l of
                        EmptyL -> None
                        ConsL x _ -> Some x
                _ -> case l of
                        EmptyL -> None
                        ConsL x xs -> index' (i -# 1#) xs
 

zip :: List a -> List b -> List (a, b)
zip xs ys = case xs of
                EmptyL -> EmptyL
                ConsL x xs' -> case ys of
                                EmptyL -> EmptyL
                                ConsL y ys' -> ConsL (x, y) (zip xs' ys')


zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys = 
  case xs of
    EmptyL -> EmptyL
    ConsL x xs' -> case ys of
                    EmptyL -> EmptyL
                    ConsL y ys' -> ConsL (f x y) (zipWith f xs' ys')

-- take i xs = case xs of
--             EmptyL -> EmptyL
--             ConsL x xs' -> case i of
 --                           0# -> EmptyL
 --                           _ -> ConsL x (take (i -# 1) xs')

itos :: Int# -> String -> String
itos n# cs
    | isTrue# (n# <# 0#) = '-' : itos' (negateInt# n#) cs
    | otherwise = itos' n# cs
    where
    itos' :: Int# -> String -> String
    itos' x# cs'
        | isTrue# (x# <# 10#) = C# (chr# (ord# '0'# +# x#)) : cs'
        | otherwise = case x# `quotRemInt#` 10# of
                      (# q, r #) ->
                          case chr# (ord# '0'# +# r) of
                          c# ->
                              itos' q (C# c# : cs')


showMyInt i = case i of MkMyInt ihash -> itos ihash ""
instance Show MyInt where
  show i  =  showMyInt i

data Compare where 
  Less :: Compare
  Greater :: Compare
  Equal :: Compare

-- | Left absorbing
instance Monoid Compare where
  identity = Equal
  mop x y = case x of
                Equal -> y
                x' -> x'

mconcat :: Monoid a => List a -> a
mconcat (EmptyL) = identity
mconcat (ConsL x xs) = x <+> (mconcat xs)



undefined :: a;
undefined = undefined

main :: IO (); main = undefined
