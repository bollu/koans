In this article, we're going to learn about _hoare logic_, 

```hs
data Atom = AConst Int | AVar String deriving (Eq)
data Exp = EAtom Atom | EAdd Atom Atom  deriving(Eq)
data Stmt = SAssign String Exp | SIf Exp Stmt Stmt | SWhile Exp Stmt  | SSkip | SAbort | SSeq Stmt Stmt deriving(Eq)
```

type Predicate = PImplies Exp Exp | PNot Exp | PEq Exp Exp | PTrue deriving(Eq)

pfalse :: Predicate; pfalse = PNot PTrue

type Hoare = (Predicate, Stmt, Predicate) -- hoare triple

type Env = [(String, Int)]
run :: Env -> Stmt -> Env; run = undefined


-- if A => B but not B => A, then B is weaker than A, since A "claims more" [has fewer models]. 
-- Eg. Group => Monoid, but not Monoid => Group, so Monoid is weaker than Group.
-- Weakest Predicate (in the universe of preconditons) is True --- is an algebraic structure that *Everything* satisfies.

wp :: Stmt -> Predicate -> Predicate
wp SSkip _ = p
wp SAabort _ = pfalse
wp (SAssign x v) r = r 
