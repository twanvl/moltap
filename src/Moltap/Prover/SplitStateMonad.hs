{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Prover.SplitStateMonad
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A state+failure+split monad
--
--------------------------------------------------------------------------------

module Moltap.Prover.SplitStateMonad
    ( SSM
    -- * Functions for this monad
    , get, put, gets, modify, abort, split, par, (<?>)
    , trace
    -- * Running computations
    , runSSM, evalSSM, execSSM
    ) where

import Control.Monad.Error
import Control.Applicative
import Data.Monoid

-- #define TRACING

import qualified Debug.Trace as D

--------------------------------------------------------------------------------
-- Generic tablaux monad
--------------------------------------------------------------------------------

-- | The SplitState monad type.
--
--   This monad supports state (of type @s@), failure (with errors of type (@e@), and a split operation.
--   State is local to branches of split, but branches are extended until the end of the computation.
data SSM s e a
    = Abort    e
    | Return   a
    | Split    (SSM s e a) (SSM s e a)
    | Get      (s -> SSM s e a)
    | Put      s (SSM s e a)
    | Trace    String (SSM s e a)

instance Functor (SSM s e) where
    fmap _ (Abort      e) = Abort e
    fmap f (Return     a) = Return (f a)
    fmap f (Split    a b) = Split (fmap f a) (fmap f b)
    fmap f (Get        a) = Get (\s -> fmap f (a s))
    fmap f (Put      s a) = Put s (fmap f a)
    fmap f (Trace    m a) = Trace m (fmap f a)

instance Monad (SSM s e) where
    return = Return
    Abort      e >>= _ = Abort e
    Return     a >>= f = f a
    Split    a b >>= f = Split (a >>= f) (b >>= f)
    Get        a >>= f = Get (\s -> a s >>= f)
    Put      s a >>= f = Put s (a >>= f)
    Trace    m a >>= f = Trace m (a >>= f)

instance Applicative (SSM s e) where
    pure = Return
    Abort      e <*> _ = Abort e
    Return     a <*> x = fmap a x
    Split    a b <*> x = Split (a <*> x) (b <*> x)
    Get        a <*> x = Get (\s -> a s <*> x)
    Put      s a <*> x = Put s (a <*> x)
    Trace    m a <*> x = Trace m (a <*> x)

instance Monoid e => MonadPlus (SSM s e) where
    mzero = Abort mempty
    mplus = Split

-- for semi-sensible Show instances
instance Show (SSM s e a) where show _ = "?"

--------------------------------------------------------------------------------
-- Special functions
--------------------------------------------------------------------------------

-- | Get the state from a SSM
get :: SSM s e s
get = Get Return

-- | Get the state from a SSM, apply a function first
gets :: (s -> s') -> SSM s e s'
gets f = Get (Return . f)

-- | Change the state in a SSM
put :: s -> SSM s e ()
put s = Put s (Return ())

-- | Apply a function to the state in a SSM
modify :: (s -> s) -> SSM s e ()
modify f = Get (\s -> put (f s))

-- | Abort the computation with an (error) result.
abort :: e -> SSM s e a
abort = Abort

-- | Split the computation,
--   abort only if both halves abort. In that case, combine the errors with 'mappend'.
split :: SSM s e a -> SSM s e a -> SSM s e a
split = Split

-- | Perform two computations \'in parallel\',
--   i.e. first do the best one, one that doesn\'t split.
par :: SSM s e () -> SSM s e () -> SSM s e ()
par x y = x >> y
{-par (Return ())   y = y
par x@(Abort   _) _ = x
par x@(Split _ _) y = y >> x -- swap order
par (Get     x)   y = Get (\s -> x s `par` y)
par (Put   s x)   y = Put s (x `par` y)
par x y = x >> y
--par (Trace m x)   y = Trace m (x `par` y)-}

-- | Trace a message for debug purposes
trace :: String -> SSM s e ()
#ifdef TRACING
trace m = Trace m (Return ())
#else
trace _ = Return ()
#endif

-- | Annotate aborts
(<?>) :: SSM s e a -> (e -> e) -> SSM s e a
Abort e <?> f = Abort (f e)
x       <?> _ = x

--------------------------------------------------------------------------------
-- Running computations
--------------------------------------------------------------------------------

-- | Run a SSM computation with the given initial state.
runSSM :: Monoid e => String -> s -> SSM s e a -> Either e (a,s)
runSSM _ s (Return     a) = Right (a,s)
runSSM _ _ (Abort      e) = Left e
runSSM w s (Get        a) = runSSM w s (a s)
runSSM w _ (Put     s' a) = runSSM w s' a
runSSM w s (Split    a b) = case (runSSM (w ++ "L") s a, runSSM (w ++ "R") s b) of
                              (Right r, _)       -> Right r
                              (_,       Right r) -> Right r
                              (Left e1, Left e2) -> Left (mappend e1 e2)
runSSM w s (Trace    m a) = D.trace (w ++ ":" ++ m) $ runSSM w s a

-- | Run a SSM computation with the given initial state.
--   Return the final state or an error.
execSSM :: Monoid e => s -> SSM s e a -> Either e s
execSSM s = fmap snd . runSSM "" s

-- | Run a SSM computation with the given initial state.
--   Return the result or an error.
evalSSM :: Monoid e => s -> SSM s e a -> Either e a
evalSSM s = fmap fst . runSSM "" s
