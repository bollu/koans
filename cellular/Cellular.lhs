At the end of this blog post, we'll know how to render pretty 
cellular automata such as these:

and the really cool algebraic structure that these possess! They turn
out to be the _dual_ of a `monad`, known as a comonad.

First, our magic incantations:


> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts          #-}
> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE PartialTypeSignatures #-}
> 
> module Main where
> -- import qualified Data.Vector as V
> import Control.Monad
> import Data.List(intercalate)
> import Data.Monoid (Any)
> import Diagrams.Prelude hiding (after)
> import Diagrams.TwoD.Layout.Grid
> import Diagrams.Core.Compile
> import Diagrams.Core.Types
> import Diagrams.Backend.CmdLine
> import Diagrams.Backend.Cairo.CmdLine
> import Diagrams.Backend.Cairo
> import Diagrams.TwoD
> import System.Random

This simulation uses the `Comonad` typeclass to model
cellular automata. There are multiple ways of looking at this algebra, and one
way to think of them is a structure that can automatically convert
"global-to-local" transforms into "global-to-global" transforms.


For example, in a cellular automata, the "global-to-local" transformation is
updating the state of _one_ Cell by reading the cell's neighbours.
The neighbour state is the global state, which is used to update the local
state of the cell. This can be thought of as the type

```haskell
CA -> Cell
```

where `CA` is the state of the full cellular automata universe, and `Cell`
is the new state of a cell. However, the question that immediately arises is
-- which cell? the answer is that, the `CA` not only encodes the state of the
cellular automata, but also a __focused cell__ which is updated.

However, this seems ridiculous, since we have simply added extra complexity
(that of focusing on a particular cell) with zero gains in benefit. 



The nice part of a `Comonad` is that if we have a structure that knows how to
do a "focused update", the `Comonad` enables us to extend this to the entire
structure.  It is called `cobind`:

    ```haskell
    cobind :: CA -> (CA -> Cell) -> CA
    ```

If we think of grid as a container of cells (or as a functor `w`), this gives us the new type


    ```haskell
    cobind :: CA -> (CA -> Cell) -> CA
    ```
 replace `CA` with `w Cell`:

    ```haskell
    cobind :: w Cell -> (w Cell -> Cell) -> w Cell
    ```

replace `Cell` with `a`:

    ```haskell
    cobind :: w a -> (w a -> a) -> w a
    ```

 generalize type even further, by allowing the output type to differ:

    ```haskell
    cobind :: w a -> (w a -> b) -> w b
    ```

Note that this rewrite exploited the fact that a `Grid` is simply a functor
(collection) of `Cell`s, and then used this to rewrite the type signature.


The type signature

    ```haskell
    cobind :: w a -> (w a -> b) -> w b
    ```
can be sharply contrasted with the monadic `>>= (bind)` as 

    ```haskell
    >>= :: m a -> (a -> m b) -> m b
    ```

So this is the "dual" of `>>=` (hence we use `w` instead of `m`). In haskell
code, this appears as:


> class Functor w => Comonad w where
>   extract :: w a -> a
>   duplicate :: w a -> w (w a)
>   cobind :: (w a -> b) -> w a -> w b
>   cobind f = fmap f . duplicate
> 


with the interpretation that it takes a global structure `w a` which is focused
on some `a` in the `w a`, and then takes a transform that updates the focused
`a` in the `w a ` to a `b`. Given these two pieces of information, the Comonad
automatically updates every single `a`, to produce an updated `w b`.


The particular data structure that implements comonad for
modelling our cellular automata is known as a `RingZipper`. This is a data
structure that provides us access to a circular arrangement of elements, with
one particular element that's currently focused. Concretely, it looks like:


> data RingZipper a = RingZipper {
>     before :: [a],
>     focus  :: a,
>     after  :: [a]
> } deriving(Eq, Ord)
> 
> instance Functor RingZipper where
>     fmap f RingZipper{..} = RingZipper {
>         before = fmap f before,
>         focus = f focus,
>         after = fmap f after
>     }
> 
> 


We're going to imagine that this already has a `Comonad` instance, and we're
simply waiting to write rules for this. So, the rules we want to write are
these:

- We have a cell which contains a particular color, which we represent with an `Int`.


> 
> data Cell = Cell { cv :: Int }



Our full simulation, called as `CA` since it's our cellular automata consists
of these cells arranged in a circular universe.



> type CA = RingZipper Cell




`stepCell` takes as input a `CA`, which remember is a circular universe that
is *focused* at a given location, and then tells us how to produce the next
*focused* cell.


> stepCell :: CA -> Cell
> stepCell s = cell'
>     where
>         cell = extract s  -- extract neighbour
>         cell' = if hasNextNeighbour (neighbours s)
>            then Cell { cv = (cv cell + 1) `mod` ctot }
>            else cell
>         hasNextNeighbour neighbours = any (\c -> cv c == ((cv cell) + 1) `mod` ctot) neighbours
> 
> -- | extract left and right neighbour from a cell.
> neighbours :: RingZipper a -> [a]
> neighbours z = [extract $ shiftLeft z, extract $ shiftRight z]


> 
> 
> makeRingZipperM :: Monad m => Int -> m a -> m (RingZipper a)
> makeRingZipperM n f = do
>     let mid = n `div` 2
>     before <- replicateM (mid - 1) f
>     after <- replicateM (n - mid + 1) f
>     focus <- f
>     return $ RingZipper {
>         before=before, 
>         focus=focus,
>         after=after
>     }
>
> lengthRingZipper :: RingZipper a -> Int
> lengthRingZipper z = length (before z) + 1 + length (after z)
> 
> focusIndexRingZipper :: RingZipper a -> Int
> focusIndexRingZipper z = length (before z)
> 
> mergeRingZipper :: RingZipper a -> [a]
> mergeRingZipper z =  before z <> [focus z] <> after z
> 
> 
> shiftLeft :: RingZipper a -> RingZipper a
> shiftLeft z = RingZipper {
>         before = before',
>         focus = focus',
>         after = after'
>     }
>     where
>         merged = mergeRingZipper z
>         focusAt' = (focusIndexRingZipper z - 1) `mod` (length merged)
>         
>         focus' = merged !!  focusAt'
>         before' =   
>             if null (before z)
>                 then init merged
>                 else before z
>         after' = 
>             if null (before z)
>                 then []
>                 else cons (focus z) (after z)
> 
> 
> shiftRight :: RingZipper a -> RingZipper a
> shiftRight z = RingZipper {
>         before = before',
>         focus = focus',
>         after = after'
>     }
>     where
>         merged  = mergeRingZipper z
>         focusAt' = (focusIndexRingZipper z + 1) `mod` (length merged)
>         
>         focus' = merged !! focusAt'
>         before' =   
>             if null (after z)
>                 then []
>                 else snoc (before z) (focus z)
>         after' = 
>             if null (after z)
>                 then tail merged
>                 else tail (after z)
> 
> 
> iterate1 :: (a -> a) -> a -> [a]
> iterate1 f x = iterate f (f x)
> 
> instance Comonad RingZipper where
>     extract RingZipper {..} = focus
> 
>     duplicate z = RingZipper {
>         before = before,
>         focus = z,
>         after = after
>     } where
>         focusAt = focusIndexRingZipper z
>         before = reverse $ iterateN focusAt shiftLeft (shiftLeft z)
>         after = iterateN (lengthRingZipper z - focusAt - 1) shiftRight (shiftRight z)
> 
> 
> 
> ctot :: Int; ctot = 5




Drawing code
-------------

This is the part that interfaces with the `diagrams` library to draw these
cellular automata.




> type MyBackend = Cairo
> renderCA :: CA -> QDiagram MyBackend V2 (N MyBackend) Any
> renderCA rz = foldr1 (|||) (map cellToDiagram $ (mergeRingZipper rz))

> cellToDiagram :: Cell -> QDiagram MyBackend V2 Double Any
> cellToDiagram Cell{cv=0, ..}  = (rect 1 4# fc (sRGB24read "#1abc9c"))
> cellToDiagram Cell{cv=1, ..} = (rect 1 4 # fc (sRGB24read "#f1c40f"))
> cellToDiagram Cell{cv=2, ..} = (rect 1 4 # fc (sRGB24read "#e67e22"))
> cellToDiagram Cell{cv=3, ..} = (rect 1 4 # fc (sRGB24read "#9b59b6"))
> cellToDiagram Cell{cv=4, ..} = (rect 1 4 # fc (sRGB24read "#2c3e50"))
> cellToDiagram Cell{..} = square 1 # fc (sRGB f (1.0 - f) 0.0) where
>                             f = (fromIntegral cv / fromIntegral ctot)
> 
> cyclic1dDim = 200
> cyclic1dTypes = 5
> 
> mkCell :: IO Cell
> mkCell = do
>   newStdGen
>   val <- getStdRandom (randomR (0, cyclic1dTypes)) :: IO Int
>   return $ Cell val
> 
> mkStart :: IO (CA)
> mkStart = do
>   rz <- makeRingZipperM cyclic1dDim mkCell
>   return $ rz
> 
> 
> type GifDelay = Int
> mkCAGif :: CA -> Int -> [(QDiagram MyBackend V2 (N MyBackend) Any, GifDelay)]
> mkCAGif ca n = zip renderedSteps [5..] where
>     renderedSteps = map renderCA cas
>     cas = take n $ iterate (cobind stepCell) ca
> 
> main :: IO ()
> main = do
>   start <- mkStart
>   let nsteps = 100
>   gifMain $ (mkCAGif start nsteps)



- [I] /home/bollu/work/functionalworks > cabal build && cabal exec cellular -- -w 512 -h30 -o foo.gif
