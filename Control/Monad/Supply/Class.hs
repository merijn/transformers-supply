{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Supply
-- Copyright   :  (C) 2013 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- See "Control.Monad.Supply" for documentation.
-------------------------------------------------------------------------------
module Control.Monad.Supply.Class (
      MonadSupply (..)
    , demand
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Supply (SupplyT)
import qualified Control.Monad.Trans.Supply as Supply
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Data.Monoid

-- | The 'MonadSupply' class provides access to the functions needed to
-- construct supply-consuming computations in a monad transformer stack.
class Monad m => MonadSupply s f m | m -> s, m -> f where
    -- | Supply a construction function with an @s@ value from the supply,
    -- the @f@ in the function's type refers to the monad wrapped by 'SupplyT'.
    supply :: (s -> f a) -> m a

    -- | Provide a non-monadic construction function with an @s@ value from the
    -- supply and automatically lift its result into the @f@ monad that
    -- 'SupplyT' wraps.
    provide :: (s -> a) -> m a

-- | Demand an @s@ value from the supply.
demand :: MonadSupply s f m => m s
demand = provide id

instance (Functor m, Monad m) => MonadSupply s m (SupplyT s m) where
    supply = Supply.supply
    provide = Supply.provide

instance MonadSupply s f m => MonadSupply s f (ContT r m) where
    supply = lift . supply
    provide = lift . provide

instance (Error e, MonadSupply s f m) => MonadSupply s f (ErrorT e m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (IdentityT m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (ListT m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (MaybeT m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (ReaderT r m) where
    supply = lift . supply
    provide = lift . provide

instance (Monoid w, MonadSupply s f m) => MonadSupply s f (LazyRWS.RWST r w s m) where
    supply = lift . supply
    provide = lift . provide

instance (Monoid w, MonadSupply s f m) => MonadSupply s f (StrictRWS.RWST r w s m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (Lazy.StateT s m) where
    supply = lift . supply
    provide = lift . provide

instance MonadSupply s f m => MonadSupply s f (Strict.StateT s m) where
    supply = lift . supply
    provide = lift . provide

instance (Monoid w, MonadSupply s f m) => MonadSupply s f (Lazy.WriterT w m) where
    supply = lift . supply
    provide = lift . provide

instance (Monoid w, MonadSupply s f m) => MonadSupply s f (Strict.WriterT w m) where
    supply = lift . supply
    provide = lift . provide
