-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Supply
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
-- [Zero and plus:] Identical to the underlying implementation (if any) of
-- 'empty' and '<|>'.
--
-- [Example type:] @'Supply' s a@ &#160; or &#160; @'SupplyT' s f a@
--
-- [Difference from "Control.Monad.Trans.Supply":] 'SupplyT' defined in this
-- module is not, and cannot be an instance of 'Monad'. See the "Applicative vs
-- Monad" section below for an in-depth explanation.
--
-- The @'Supply' s a@ applicative represents a computation that consumes a
-- supply of @s@'s to produce a value of type @a@. One example use is to
-- simplify computations that require the generation of unique names. The
-- 'Supply' applicative can be used to provide a stream of unique names to such
-- a computation.
-------------------------------------------------------------------------------
module Control.Applicative.Supply (
    -- ** Applicative vs Monad SupplyT
    -- $why-applicative

    -- ** Applicative Transformer?!
    -- $why-transformer

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
import Data.Functor.Identity

-- $why-applicative
-- 𝐓𝐋;𝐃𝐑: If you're wrapping a 'Monad', use "Control.Monad.Trans.Supply".
--
-- As mentioned above, the implementation of
-- 'Control.Applicative.Supply.SupplyT' in this module is not an instance of
-- 'Monad' and cannot be made one. While the implementation in
-- "Control.Monad.Trans.Supply" can be made a 'Monad' instance, this extra
-- power comes at a cost.
--
-- Specifically, 'Control.Monad.Trans.Supply.SupplyT' cannot be made an
-- instance of 'Applicative', unless the wrapped type is a 'Monad'. That is,
-- making @'SupplyT s f a'@ an 'Applicative' requires that @f@ is a 'Monad'. As
-- a result, we cannot wrap something that is only an 'Applicative' without
-- losing the ability to compose computations in an applicative style.
--
-- The 'SupplyT' defined in this module exists to compliment the
-- 'Control.Monad.Trans.Supply.SupplyT' with a transformer than /can/ have an
-- 'Applicative' instance when the wrapped type is not a 'Monad', at the cost
-- of not being able to wrap a 'Monad'.
--
-- If the type you're wrapping has a 'Monad' instance, you should use the
-- transformer from "Control.Monad.Trans.Supply" as it is more powerful than
-- this one.

-- $why-transformer
-- You might be wondering, \"Why define 'SupplyT' in a transformer style? You
-- can already compose 'Applicative's using "Data.Functor.Compose"!\". The main
-- reason for this is that, this way people don't have to deal with newtype
-- (un)wrapping, and I don't have to come up with a way to disambiguate the
-- transformed and untransformed versions of 'demand' and 'provide', as they
-- now work for both the transformed and untransformed case.

-- | The Supply applicative.
--
-- Computations consume values of type @s@ from a supply of values.
--
-- 'pure' ignores the supply of values, while '<*>' passes the supply to the
-- second argument after the first argument is done consuming values.
type Supply s a = SupplyT s Identity a

-- | The Supply transformer.
--
-- Composes Supply with an underlying applicative, identical to using
-- @'Data.Functor.Compose' 'Supply' f a@, but this implementation avoids the
-- need to explicitly wrap 'demand' in 'pure' everywhere.
--
-- The resulting SupplyT value has an 'Alternative' instance if the underlying
-- applicative has an 'Alternative' instance.
data SupplyT s f a = Done (f a) | More (s -> SupplyT s f a)

instance Functor f => Functor (SupplyT s f) where
    fmap f (Done x) = Done (f <$> x)
    fmap f (More g) = More $ fmap f . g

instance Applicative f => Applicative (SupplyT s f) where
    pure = Done . pure
    (Done f) <*> (Done x) = Done (f <*> x)
    (More f) <*> x = More $ \s -> f s <*> x
    f <*> (More g) = More $ \s -> f <*> g s

instance Alternative f => Alternative (SupplyT s f) where
    empty = Done empty
    (Done x) <|> (Done y) = Done (x <|> y)
    (More f) <|> x = More $ \s -> f s <|> x
    x <|> (More f) = More $ \s -> x <|> f s

-------------------------------------------------------------------------------
-- Supply Operations

-- | Supply a construction function with an @s@ value from the supply.
supply :: (s -> f a) -> SupplyT s f a
supply f = More $ Done . f

-- | Supply a non-applicative construction function with an @s@ value from
-- the supply and automatically lift its result into the @f@ applicative that
-- 'SupplyT' wraps.
provide :: Applicative f => (s -> a) -> SupplyT s f a
provide f = supply (pure . f)

-- | Demand an @s@ value from the supply.
demand :: Applicative f => SupplyT s f s
demand = provide id

-- | Change the type of values consumed by a 'Supply' computation.
withSupply :: (s' -> s) -> Supply s a -> Supply s' a
withSupply = withSupplyT

-- | Change the type of values consumed by a 'SupplyT' computation.
withSupplyT :: (s' -> s) -> SupplyT s f a -> SupplyT s' f a
withSupplyT _ (Done x) = Done x
withSupplyT f (More g) = More $ withSupplyT f . g . f

-------------------------------------------------------------------------------
-- Running Supply Computations

-- | Run a supply consuming computation, using a generation function and
-- initial value to compute the values consumed by the 'Supply' computation.
runSupply :: Supply s a -> (s -> s) -> s -> a
runSupply sink gen s = runIdentity $ runSupplyT sink gen s

-- | Run a supply consuming computation, using a generation function and
-- initial value to compute the values consumed by the 'SupplyT' computation.
runSupplyT :: SupplyT s f a -> (s -> s) -> s -> f a
runSupplyT (Done result) _ _ = result
runSupplyT (More f) gen s = runSupplyT (f s) gen (gen s)

-- | Feed a supply consuming computation from a list until the computation
-- finishes or the list runs out. If the list does not contain sufficient
-- elements, @runListSupply@ returns uncompleted computation.
runListSupply :: Supply s a -> [s] -> Either (Supply s a) a
runListSupply sink l = runIdentity <$> runListSupplyT sink l

-- | Feed a supply consuming computation from a list until the computation
-- finishes or the list runs out. If the list does not contain sufficient
-- elements, @runListSupplyT@ returns uncompleted computation.
runListSupplyT :: SupplyT s f a -> [s] -> Either (SupplyT s f a) (f a)
runListSupplyT (Done result) _ = Right result
runListSupplyT (More f) (s:ss) = runListSupplyT (f s) ss
runListSupplyT (More f) [] = Left (More f)

-- | Feed a supply consuming computation from a monadic action until the
-- computation finishes.
runMonadSupply :: Monad m => Supply s a -> m s -> m a
runMonadSupply sink src = liftM runIdentity $ runMonadSupplyT sink src

-- | Feed a supply consuming computation from a monadic action until the
-- computation finishes.
runMonadSupplyT :: Monad m => SupplyT s f a -> m s -> m (f a)
runMonadSupplyT (Done result) _ = return result
runMonadSupplyT (More f) src = src >>= \s -> runMonadSupplyT (f s) src
