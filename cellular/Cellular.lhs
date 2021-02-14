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
> 
> type MyBackend = Cairo
> 
> class Functor w => Comonad w where
>   extract :: w a -> a
>   duplicate :: w a -> w (w a)
>   cobind :: (w a -> b) -> w a -> w b
>   cobind f = fmap f . duplicate
> 
> type Dim = Int
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
> instance Show a => Show (RingZipper a) where
>     show z = 
>       "|"<> showElems (before z) 
>          <> showCenter (focus z) 
>          <> showElems (after z) 
>          <> "|" where
>             showElems l = intercalate " " (map show l)
>             showCenter x = " (" <> show x <> ")  "
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
> neighbours :: RingZipper a -> [a]
> neighbours z = [extract $ shiftLeft z, extract $ shiftRight z]
> 
> 
> makeRingZipperM :: Monad m => Dim -> m a -> m (RingZipper a)
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
> 
> data Cell = Cell { cv :: Int }
> type Cyclic1D = RingZipper Cell
> 
> stepCell :: Cyclic1D -> Cell
> stepCell s = cell'
>     where
>         cell = extract s 
>         cell' = if hasNextNeighbour (neighbours s)
>            then Cell { cv = (cv cell + 1) `mod` ctot }
>            else cell
>         hasNextNeighbour neighbours = any (\c -> cv c == ((cv cell) + 1) `mod` ctot) neighbours
> 
> renderCA :: Cyclic1D -> QDiagram MyBackend V2 (N MyBackend) Any
> renderCA rz = foldr1 (|||) (map cellToDiagram $ (mergeRingZipper rz))
> 
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
> mkStart :: IO (Cyclic1D)
> mkStart = do
>   rz <- makeRingZipperM cyclic1dDim mkCell
>   return $ rz
> 
> 
> type GifDelay = Int
> mkCAGif :: Cyclic1D -> Int -> [(QDiagram MyBackend V2 (N MyBackend) Any, GifDelay)]
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
