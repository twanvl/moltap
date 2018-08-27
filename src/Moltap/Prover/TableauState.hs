{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Prover.TableauState
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The state type for tableau computations.
--
--------------------------------------------------------------------------------

module Moltap.Prover.TableauState
    ( module Moltap.Prover.SplitStateMonad
    -- * Tableau and state types
    , GenTableauLabel(..)
    , GenTableau(..)
    , GenTableauState(..), emptyTableauState
    , GenTableauM, GenTableauM_
    -- * Functions on the state
    -- ** Navigating the tree
    , tabLocalDown, tabLocalUpIf
    -- ** Adding formulas
    , tabAddUniv, tabMemo
    -- ** Testing for axioms
    , tabWrapIfAxiom, tabWhenAxiom
    -- ** Information on the current world
    , tabGetAgents, tabGetFormula
    -- ** Extracting results
    , tabGraph
    ) where

import Moltap.Prover.SplitStateMonad
import Moltap.Base.Syntax (Formula)
import Moltap.Base.Agents
import Moltap.Util.TreeZipper (TreeZipper)
import qualified Moltap.Util.TreeZipper as Z

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Data.Monoid

--------------------------------------------------------------------------------
-- The tableau/sequent state type
--------------------------------------------------------------------------------

-- | Tableau labels
data GenTableauLabel p = Label
    { lblAgents :: Agents
    , lblSeed   :: p
    }
  deriving (Show)

-- | A tableau for a single world
data GenTableau p e = Tableau
    { -- | Things we have already seen
      tabSeen      :: Map Formula p
      -- | Things that hold in all reachable worlds
    , tabUniversal :: [(Agents, GenTableauM_ p e)]
    }
  deriving (Show)

-- | A tree of worlds with tableaux
type GenTableauTree p e = TreeZipper (GenTableauLabel p) (GenTableau p e)

-- | State in a tableau computation,
--   generalized over proof type.
data GenTableauState p e = TableauState
    { -- | The tableaux for the current world
      tabZipper :: GenTableauTree p e
      -- | Axioms that hold for different agents
    , tabAxioms :: Axioms
    }
  deriving (Show)

type GenTableauM  p e a = SSM (GenTableauState p e) e a
type GenTableauM_ p e = GenTableauM p e ()

-- | The initial tableau state
emptyTableau :: GenTableau p e
emptyTableau = Tableau Map.empty []

-- | The initial tableau state
emptyTableauState :: Axioms -> GenTableauState p e
emptyTableauState ax = TableauState (Z.fromRoot emptyTableau) ax

-- | Convert the tableau state to a graph of tableaux
tabGraph :: GenTableauState p e -> [(Int, GenTableau p e, [(Int, GenTableauLabel p)])]
tabGraph = Z.toGraph . Z.toTree . tabZipper

--------------------------------------------------------------------------------
-- State operations
--------------------------------------------------------------------------------

-- | Extract the zipper state
getZ :: GenTableauM p e (GenTableauTree p e)
getZ = gets tabZipper
-- | Replace the zipper state
putZ :: GenTableauTree p e -> GenTableauM_ p e
putZ z' = modify $ \s -> s { tabZipper = z' }
-- | Change the zipper state
modifyZ :: (GenTableauTree p e -> GenTableauTree p e) -> GenTableauM_ p e
modifyZ f = modify $ \s -> s { tabZipper = f (tabZipper s) }

-- | Extract the tableau for the current world
getV :: GenTableauM p e (GenTableau p e)
getV = gets (Z.getValue . tabZipper)
-- | Change the tableau of the current world
putV :: GenTableau p e -> GenTableauM_ p e
putV v' = modifyZ (Z.setValue v')


-- | Get the seed formula for the current world (not at the root)
tabGetFormula :: GenTableauM p e p
tabGetFormula = gets (lblSeed . Z.getLabel . tabZipper)
-- | Get the agents for the current world (not at the root)
tabGetAgents :: GenTableauM p e Agents
tabGetAgents = gets (lblAgents . Z.getLabel . tabZipper)

--------------------------------------------------------------------------------
-- Tableau/sequent state type
--------------------------------------------------------------------------------

-- | If an axiom holds, wrap a, else don't change anything
tabWrapIfAxiom :: (AxiomSet -> Bool) -> Agents -> (GenTableauM_ p e -> GenTableauM_ p e) -> GenTableauM_ p e -> GenTableauM_ p e
tabWrapIfAxiom test a wrap m = do
    ax <- gets tabAxioms
    if hasAxiom test a ax then wrap m else m

-- | If an axiom holds, perform an axiom, otherwise do nothing
tabWhenAxiom :: (AxiomSet -> Bool) -> Agents -> GenTableauM_ p e -> GenTableauM_ p e
tabWhenAxiom test a m = do
    ax <- gets tabAxioms
    if hasAxiom test a ax then m else return ()

--------------------------------------------------------------------------------
-- Zipper: low level interface
--------------------------------------------------------------------------------

-- | Go down in the hierarchical tableau for the given agents.
--   Instantiates universals when needed.
tabGoDownNew :: (Show p, Monoid e) => Agents -> p -> GenTableauM_ p e
tabGoDownNew agents p = do
    trace $ "down new for " ++ show agents
    zipper <- getZ
    -- instantiate universals for this agent set,
    -- for partially matching universals we split the agent set
    forMatchingAgents agents (tabUniversal $ Z.getValue zipper) $
       \a' acts -> do putZ $ Z.downNew (Label a' p) emptyTableau zipper
                      sequence_ acts
    trace $ "done down for " ++ show agents

-- | Go back up the zipper
tabGoBackUp :: GenTableauM_ p e
tabGoBackUp = modifyZ Z.up

--------------------------------------------------------------------------------
-- Zipper behaviour
--------------------------------------------------------------------------------

-- | Perform a tableau computation in a new world reachable from the current one by the given agents.
tabLocalDown :: (Show p, Monoid e) => Agents -> p -> GenTableauM_ p e -> GenTableauM_ p e
tabLocalDown agents p x = do
    tabGoDownNew agents p
    x
    trace $ "back up for " ++ show agents
    tabGoBackUp

-- | Perform a tableau computation in a world from which the current one is reachable,
--   and it is reached by an agent matching the test.
tabLocalUpIf :: Monoid e
             => (AxiomSet -> Bool) -- ^ required axioms
             -> Agents             -- ^ agents to match
             -> GenTableauM_ p e   -- ^ action to perform locally
             -> GenTableauM_ p e   -- ^ action to perform otherwise
             -> GenTableauM_ p e
tabLocalUpIf ax agents x y = do
    st <- get
    let z = tabZipper st
    if not (Z.isRoot z) && lblAgents (Z.getLabel z) == agents && hasAxiom ax agents (tabAxioms st)
      then do putZ (Z.up z)
              x
              z' <- getZ
              putZ (Z.downTo (Z.name z) z')
      else y

-- | Add a universal, instantiate it for matching worlds
tabAddUniv :: (Monoid e, Show p) => Agents -> GenTableauM_ p e -> GenTableauM_ p e
tabAddUniv a u = do
    trace $ "add univ for " ++ show a
    -- add to universal list
    tableau <- getV
    putV tableau{ tabUniversal = (a,u) : tabUniversal tableau }
    -- instantiate for existing reachable worlds.
    -- if a world matches partially, split.
    z <- getZ
    case Z.downFirst z of
      Nothing -> return () -- no worlds
      Just z' -> instForAll z'
    trace $ "done insting univ for " ++ show a
 where
    instForAll z
        | isNoAgents intersection = next z -- agent sets don't match
        | isNoAgents difference   = putZ z >> u >> getZ >>= next
        | otherwise               = undefined split -- some agents match
                                    (let z' = Z.setLabel (Label intersection p) z in putZ z' >> u >> getZ >>= next)
                                    (let z' = Z.setLabel (Label difference   p) z in next z')
      where Label agents p = Z.getLabel z
            (intersection, difference) = intersectAgents agents a
    -- Move on to the next reachable world
    next z = case Z.next z of
      Nothing -> putZ (Z.up z) -- this was the last world
      Just z' -> instForAll z'


-- | Memoize a formula.
--   If the formula was already seen perform 'old' to merge the possible conflict.
--   Otherwise memoize it and perform 'new'.
tabMemo :: Show p =>
           (Formula -> p -> p -> GenTableauM p e a) -- ^ The formula was already processed before
        -> (Formula -> p -> GenTableauM p e a)      -- ^ The formula was not processed before
        -> Formula -> p                             -- ^ Formula and proof
        -> GenTableauM p e a
tabMemo old new f p = do
    tableau <- getV
    case Map.lookup f (tabSeen tableau) of
        Just p' -> do old f p p'
        Nothing -> do putV tableau{ tabSeen = Map.insert f p (tabSeen tableau) }
                      trace $ "addmemo " ++ show f
                      new f p
