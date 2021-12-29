data Atom = Const Int | Var String
data Exp = EAtom Atom | Add Atom Atom 
data Stmt = Assign String Exp | If Exp Block Block  | While Exp Block
type Block = [Stmt]

type Rel = 
