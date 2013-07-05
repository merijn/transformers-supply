-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Supply
-- Copyright   :  (C) 2013 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- [Computation type:] Computations that require a supply of values.
--
-- [Binding strategy:] Applicative values are functions that consume an input
-- from a supply to produce a value.
--
-- [Useful for:] Providing a supply of unique names or other values to
-- computations needing them.
--
-- [Zero and plus:] Identical to the underlying implementations (if any) of
-- 'empty', '<|>', 'mzero' and 'mplus'.
--
-- [Example type:] @'Supply' s a@ &#160; or &#160; @'SupplyT' s f a@
--
-- [Difference from "Control.Applicative.Supply":] The 'Applicative' instance
-- of 'SupplyT' defined in this module requires that the wrapped type is an
-- instance of 'Monad'. See the "Applicative vs Monad" section below for an
-- in-depth explanation.
--
-- The @'Supply' s a@ monad represents a computation that consumes a supply of
-- @s@'s to produce a value of type @a@. One example use is to simplify
-- computations that require the generation of unique names. The 'Supply' monad
-- can be used to provide a stream of unique names to such a computation.
-------------------------------------------------------------------------------
module Control.Monad.Trans.Supply (
    -- ** Applicative vs Monad SupplyT
    -- $why-monad

    -- * Supply and SupplyT Type
      Supply
    , SupplyT
    -- * Supply Operations
    , supply
    , provide
    , demand
    , withSupply
    , withSupplyT
    -- * Running Supply Computations
    , runSupply
    , runSupplyT
    , runListSupply
    , runListSupplyT
    , runMonadSupply
    , runMonadSupplyT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity

-- $why-monad
-- 𝐓𝐋;𝐃𝐑: Ignore "Control.Applicative.Supply" if you're wrapping a 'Monad'.
--
-- A 'Monad' instance of 'Supply' results in 'Supply' actions that can be
-- executed conditionally (after all, that's what 'Monad's are for!),
-- implementing the 'SupplyT' in a way that allows this results in an important
-- restriction, it is impossible to define an 'Applicative' instance for
-- @'SupplyT' s m a@ without a 'Monad' instance for @m@! As a result, it is not
-- possible to use this transformer to wrap something that is only
-- 'Applicative' and not 'Monad' and still get an 'Applicative' instance back.
-- To solve this issue, a slightly different transformer is implemented in
-- "Control.Applicative.Supply", which does allow this!
--
-- Since it cannot be made an instance of 'Monad', the
-- 'Control.Applicative.Supply.SupplyT' transformer from
-- "Control.Applicative.Supply" is less powerful than the one defined here. If
-- you're wrapping a 'Monad', use the transformer defined in this module,
-- instead of the one defined in "Control.Applicative.Supply".

-- | The Supply monad.
--
-- Computations consume values of type @s@ from a supply of values.
--
-- 'return' ignores the supply of values, while '>>=' passes the supply to the
-- second argument after the first argument is done consuming values.
type Supply s a = SupplyT s Identity a

-- | The Supply transformer.
--
-- Composes Supply with an underlying monad, allowing it to be used monad in
-- transformer stacks.
--
-- The resulting SupplyT value has 'Alternative' and 'MonadPlus' instances if
-- the underlying monad has such these instances.
newtype SupplyT s m a = SupplyT { unwrapSupplyT :: m (Consumer s m a) }

data Consumer s m a = Done a | More (s -> SupplyT s m a)

instance Functor f => Functor (Consumer s f) where
    fmap f (Done a) = Done (f a)
    fmap f (More g) = More $ fmap f . g

instance Functor f => Functor (SupplyT s f) where
    fmap f (SupplyT x) = SupplyT (fmap (fmap f) x)

instance (Functor m, Monad m) => Applicative (SupplyT s m) where
    pure = SupplyT . return . Done
    (<*>) = ap

instance (Functor m, Monad m) => Monad (SupplyT s m) where
    return = pure
    SupplyT m >>= f = SupplyT $ m >>= \v -> case v of
        Done a -> unwrapSupplyT (f a)
        More g -> return . More $ g >=> f

instance MonadTrans (SupplyT s) where
    lift = SupplyT . liftM Done

instance (Alternative m, Monad m) => Alternative (SupplyT s m) where
    empty = SupplyT empty
    SupplyT x <|> SupplyT y = SupplyT (x <|> y)

instance (Functor m, MonadPlus m) => MonadPlus (SupplyT s m) where
    mzero = SupplyT mzero
    SupplyT x `mplus` SupplyT y = SupplyT (x `mplus` y)

instance (Functor m, MonadIO m) => MonadIO (SupplyT s m) where
    liftIO = lift . liftIO

-------------------------------------------------------------------------------
-- Supply Operations

-- | Supply a construction function with an @s@ value from the supply.
supply :: Monad m => (s -> m a) -> SupplyT s m a
supply f = SupplyT . return . More $ SupplyT . liftM Done . f

-- | Supply a non-monadic construction function with an @s@ value from the
-- supply and automatically lift its result into the @m@ monad that 'SupplyT'
-- wraps.
provide :: Monad m => (s -> a) -> SupplyT s m a
provide f = supply (return . f)

-- | Demand an @s@ value from the supply.
demand :: Monad m => SupplyT s m s
demand = provide id

-- | Change the type of values consumed by a 'Supply' computation.
withSupply :: (s' -> s) -> Supply s a -> Supply s' a
withSupply = withSupplyT

-- | Change the type of values consumed by a 'SupplyT' computation.
withSupplyT :: Functor f => (s' -> s) -> SupplyT s f a -> SupplyT s' f a
withSupplyT f (SupplyT m) = SupplyT (fmap go m)
    where go (Done x) = Done x
          go (More g) = More $ withSupplyT f . g . f

-------------------------------------------------------------------------------
-- Running Supply Computations

-- | Run a supply consuming computation, using a generation function and
-- initial value to compute the values consumed by the 'Supply' computation.
runSupply :: Supply s a -> (s -> s) -> s -> a
runSupply act gen = runIdentity . runSupplyT act gen

-- | Run a supply consuming computation, using a generation function and
-- initial value to compute the values consumed by the 'SupplyT' computation.
runSupplyT :: Monad m => SupplyT s m a -> (s -> s) -> s -> m a
runSupplyT (SupplyT m) gen s = join $ liftM go m
    where go (Done x) = return x
          go (More f) = runSupplyT (f s) gen (gen s)

-- | Feed a supply consuming computation from a list until the computation
-- finishes or the list runs out. If the list does not contain sufficient
-- elements, @runListSupply@ returns uncompleted computation.
runListSupply :: Supply s a -> [s] -> Either (Supply s a) a
runListSupply sink l = runIdentity $ runListSupplyT sink l

-- | Feed a supply consuming computation from a list until the computation
-- finishes or the list runs out. If the list does not contain sufficient
-- elements, @runListSupplyT@ returns uncompleted computation.
runListSupplyT :: Monad m => SupplyT s m a -> [s] -> m (Either (SupplyT s m a) a)
runListSupplyT (SupplyT m) [] = return $ Left (SupplyT m)
runListSupplyT (SupplyT m) (s:ss) = join $ liftM go m
    where go (Done x) = return (Right x)
          go (More f) = runListSupplyT (f s) ss

-- | Feed a supply consuming computation from a monadic action until the
-- computation finishes.
runMonadSupply :: Monad m => Supply s a -> m s -> m a
runMonadSupply (SupplyT m) src = go $ runIdentity m
    where go (Done x) = return x
          go (More f) = src >>= \s -> runMonadSupply (f s) src

-- | Feed a supply consuming computation from a monadic action until the
-- computation finishes.
runMonadSupplyT :: Monad m => SupplyT s m a -> m s -> m a
runMonadSupplyT (SupplyT m) src = join $ liftM go m
    where go (Done x) = return x
          go (More f) = src >>= \s -> runMonadSupplyT (f s) src
