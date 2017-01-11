{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Maybe
import Data.Monoid 
import Control.Monad.Writer

type Log = String
type Data = Int

getpasswd :: IO (Maybe String)
getlogin :: IO (Maybe String)

authenticate :: IO Bool
authenticate = do -- IO context
          passwd <- getpasswd :: Maybe String 
          login <- getlogin  :: Maybe String
          case passwd of
              Nothing -> return False
              Just p -> 
                        case login of
                            Nothing -> return False
                            Just l -> return $ p == "admin" && l == "admin"

user :: IO ()
user = do
  val <- authenticate :: Bool

-- ===

getpasswd' :: (MaybeT IO) String
getlogin' :: (MaybeT IO) String

authenticate' :: MaybeT IO Bool
  passwd <- getpasswd :: String
  login  <- getlogin :: String

  return $ passwd == "admin" && login == "admin" 

authenticate_bool :: IO Bool
authenticate_bool = do -- IO context
  mb :: Maybe Bool <- unMaybeT authenticate' :: IO (Maybe bool)
  case mb of
    Nothing -> return False
    Just v -> return v

user' :: IO ()
user' = do
  val <- authenticate_bool
  

-- ===

data MaybeT m a = MaybeT { unMaybeT :: m (Maybe a) } 

instance Monad m => (Monad (MaybeT m)) where
  return :: a -> MaybeT m a
  return a = return (Just a)
  
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  mta >>= f = do -- in the context of "m"
              (ma' :: Maybe a) <- ((unMaybeT mta) :: m (Maybe a))
              case ma' of
                Nothing -> MaybeT (return 
                                      (Nothing :: Maybe b)
                                  ) :: m (Maybe b))
                          :: MaybeT m b
            
                Just a' -> unMaybeT (f a' :: MaybeT m b)
                            ::  
x :: m (Maybe a)
f :: a -> m (Maybe b)
z :: m (Maybe b)

x >>= f = do -- with respect to m
            mayx' <- x -- Maybe a
            case mayx' of
              Nothing -> return Nothing
              Just x' -> f x'

                       +------------+
                       | MaybeT m a |
  +-----------+        +------------+
  |     m     |        |     m      |
  | +-------+ |  |..>  | +-------+  |
  | | Maybe | |  |..>  | | Maybe |  |
  | +-------+ |        | +-------+  |
  +-----------+        +------------+
 -}
 

State s a = State { statefn :: s -> (a, s) } 




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

{-
Writer ->> MaybeT
Maybe ->> WriterT
-}
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
seedmw = return 6

mainmw :: IO ()
mainmw = do
  let writer = runMaybeT (seedmw >>= blockOddMw >>= logMw)
  let aw = runWriter writer
  print aw

