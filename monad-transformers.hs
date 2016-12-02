{-# LANGUAGE NewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- import Control.Monad.Trans.Maybe
import Data.Monoid 
import Control.Monad.Writer

type Log = String
type Data = Int


data MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f mt = MaybeT {
    runMaybeT = (fmap . fmap) f (runMaybeT mt)
  }
  

-- =*= WriterT Maybe =*= 

seedwm :: WriterT Log Maybe Data
seedwm = return 5

logWm :: Data -> WriterT Log Maybe ()
logWm a = do
  if a `mod` 2 == 0 then
    tell (show a <> ": even | ")
  else
    tell (show a <> ": odd | ")

blockOddWm :: Data -> WriterT Log Maybe Data
blockOddWm d = if d `mod` 2 == 1
  then tell "blocking odd | " >> WriterT Nothing
  else tell "allowing even | " >> return d


mainwm :: IO ()
mainwm = do
  print $ runWriterT (seedwm >>= blockOddWm >>= logWm)

-- =*= MonadT Writer =*=
blockOddMw :: Data -> MaybeT (Writer Log) Data
blockOddMw d = 
  MaybeT $ 
    if d `mod` 2 == 1
    then do
      tell "blocking odd | "
      return Nothing
    else do
      tell "allowing even | "
      return $ (Just d)

logMw :: Data -> MaybeT (Writer Log) Data
logMw d = lift $ 
  if d `mod` 2 == 1 
    then do
        tell $ (show d) <> ": odd | " 
        return d
    else do
        tell $ (show d) <> ": even | "
        return d
seedmw :: MaybeT (Writer Log) Int
seedmw = return 5

mainmw :: IO ()
mainmw = do
  let writer = runMaybeT (seedmw >>= blockOddMw >>= logMw)
  let aw = runWriter writer
  print aw

