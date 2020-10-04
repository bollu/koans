data Seq a = Nil | Unit a | More (Some a) (Seq (Tuple a)) (Some a)
data Some a = One a | Two a a | Three a a a
data Tuple a = Pair a a | Triple a a a
