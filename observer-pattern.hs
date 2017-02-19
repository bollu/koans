{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
import Control.Applicative
import Data.Traversable


data Observer m e where
  MkObserver :: (e -> Maybe a) -> (a -> m ()) ->  Observer m e

data Event = KeyEvent Char | 
             MouseEvent (Int, Int)

data Dispatcher m e = Dispatcher {
  observers :: [Observer m e]
}


runEvent :: Applicative m => Dispatcher m e -> e -> m () 
runEvent d e = (for (observers d) run) *> pure () where
  run (MkObserver handler action) = 
    case handler e of
        Just (a) -> action a
        Nothing -> pure ()


