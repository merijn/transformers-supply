-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Supply
-- Copyright   :  (C) 2013 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  non-portable
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
-- The @'Supply' s a@ monad represents a computation that consumes a supply of
-- @s@'s to produce a value of type @a@. One example use is to simplify
-- computations that require the generation of unique names. The 'Supply' monad
-- can be used to provide a stream of unique names to such a computation.
-------------------------------------------------------------------------------
module Control.Monad.Supply (
    -- * The MonadSupply Class
      MonadSupply (..)
    , demand
    -- * The Supply Monad
    , Supply
    , withSupply
    , runSupply
    , runListSupply
    , runMonadSupply
    -- * The Supply Monad Transformer
    , SupplyT
    , withSupplyT
    , runSupplyT
    , runListSupplyT
    , runMonadSupplyT
    , module Control.Monad
    , module Control.Monad.Trans
    ) where

import Control.Monad.Supply.Class

import Control.Monad.Trans.Supply
    ( Supply
    , SupplyT
    , withSupply
    , withSupplyT
    , runSupply
    , runSupplyT
    , runListSupply
    , runListSupplyT
    , runMonadSupply
    , runMonadSupplyT
    )

import Control.Monad
import Control.Monad.Trans
