{-# LANGUAGE UnicodeSyntax, BangPatterns, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances, LambdaCase #-}
module Control.Monad.Warning
       (
         errorToWarning
       , errorsToWarnings
       , WarningT(..)
       , MonadWarning(..)
       , module Control.Monad.Except
       , module Control.Monad.Writer
       )
       where

import Control.Applicative
import Control.Monad.Except
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Monoid

newtype WarningT w e m a = WarningT { runWarningT ∷ w → m (w, Either e a) }

class (Monad m) ⇒ MonadWarning w e m | m → w e where
  warning ∷ w → m ()
  throwW  ∷ e → m a
  catchW  ∷ m a → (e → m a) → m a

instance (Functor m) ⇒ Functor (WarningT w e m) where
  fmap f a = WarningT $ \w → let f' (w', a') = (w', (fmap f) a')
                             in fmap f' $ runWarningT a w

instance (Applicative m) ⇒ Applicative (WarningT w e m) where
  pure a = WarningT $ \w → pure (w, Right a)
  f <*> a = undefined
  -- f <*> a = WarningT $ \w → runWarningT f w 
  --     (w', f') = 
  --     (w'', a') = runWarningT a w'
  --   in case (f', a') of
  --     (Right f'', Right a'') → undefined --runWarningT (f'' a'') w''
  --     (Left l, _) → pure (w', Left l)
  --     (_, Left l) → pure (w'', Left l)

instance (Monad m, Monoid w) ⇒ Monad (WarningT w e m) where
  return a = WarningT $ \w → return (w, Right a)
  
  a >>= b = WarningT $ \w → do
    (w', e) ← runWarningT a w
    case e of
     Right r → runWarningT (b r) w'
     Left l → return (w', Left l)

  fail = WarningT . fail

instance (Monad m, Monoid w) ⇒ MonadWarning w e (WarningT w e m) where
  warning w' = WarningT $ \w → return (w `mappend` w', Right ())

  throwW e = WarningT $ \w → return (w, Left e)
  
  catchW a f = WarningT $ \w → do
    (w', e) ← runWarningT a w
    case e of
     Right e' → return (w', Right e')
     Left e' → runWarningT (f e') w'

instance (MonadWarning w e m) ⇒ MonadError e m where
  throwError = throwW
  catchError = catchW

instance (Monoid w, MonadWarning w e m) ⇒ MonadWriter w m where
  tell = warning

instance MonadTrans (WarningT w e) where
  lift a = WarningT $ \w → do
    a' ← a
    return $ (w, Right a')

instance (Monoid w, MonadIO m) ⇒ MonadIO (WarningT w e m) where
  liftIO = lift . liftIO

instance (MonadReader r m, Monoid w) ⇒ MonadReader r (WarningT w e m) where
  ask = lift ask
  -- TODO: Check and test it.
  local f a = WarningT $ \w → local f $ runWarningT a w
  
errorToWarning ∷ (Monoid w, MonadWarning w e m) ⇒ (e → w) → (e → m a) → m a → m a
errorToWarning f g a = catchW a (\e → warning (f e) >> g e)

errorsToWarnings ∷ (Monoid w, MonadWarning w e m) ⇒ (e → w) → [m a] → m [a]
errorsToWarnings f = foldl go (return [])
  where go r a = errorToWarning f (const r) $ do
          a' ← a
          r' ← r
          return $ a' : r'
