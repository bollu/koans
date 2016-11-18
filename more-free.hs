{-# LANGUAGE GADTs #-}

import Control.Monad.Operational
import Control.Monad.Prompt
--import Control.Monad.Free.VanLaarhoven

-- operational usage

data StackInstruction a where
   Push :: Int -> StackInstruction ()
   Pop :: StackInstruction Int

push :: Int -> StackProgram ()
push x = singleton $ Push x

pop :: StackProgram Int
pop = singleton $ Pop

type StackProgram a = Program StackInstruction a
type Stack b = [b]

{-
interpret :: StackProgram a -> (Stack a -> a)
interpret = eval . view
   where
   eval :: ProgramView StackInstruction a -> (Stack Int -> a)
   eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
   eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
   eval (Return a)       stack     = a


program = do
  push 10
  push 20
  x <- pop
  y <- pop
  push (x + y)
  pop

-}

data TMInstr a where
    WriteTM :: Int -> TMInstr ()
    LeftTM :: TMInstr ()
    RightTM :: TMInstr ()
    ReadTM :: TMInstr Int

type TM a = Program TMInstr a
type Tape a = ([a], a, [a])

readtape :: Tape a -> a
readtape (_, a, _) = a

writetape :: Tape a -> a -> Tape a
writetape (left, _, right) a = (left, a, right)

lefttape :: Tape a -> Tape a
lefttape  ((l:ls), a, right) = (ls, l, a:right)

righttape :: Tape a -> Tape a
righttape (left, a, (r:rs)) = (a:left, r, rs)

interprettm :: TM a -> Tape Int -> a
interprettm = eval . view
    where
    eval :: ProgramView TMInstr a -> (Tape Int -> a)
    eval ((WriteTM a) :>>= is) tape = interprettm (is ()) (writetape tape a)
    eval (LeftTM :>>= is) tape = interprettm (is ()) (lefttape tape)
    eval (RightTM :>>= is) tape = interprettm (is ()) (righttape tape)
    eval (ReadTM :>>= is) tape = interprettm (is (readtape tape)) tape
    eval (Return a) tape = a


-- using prompt
