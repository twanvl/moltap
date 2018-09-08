{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Base.Agents
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Agents, sets of agents and operations on these sets.
--
--------------------------------------------------------------------------------

module Moltap.Base.Agents
    ( -- * Agents
      Agent
      -- * Agent sets
    , Agents(..)
      -- ** Construction
    , listAgents, allAgents
      -- ** Basic properties
    , isNoAgents, isAllAgents
    , showAgents
      -- ** Set operations
    , unionAgents, intersectAgents
      -- ** Matching agent sets
    , splitMatchingAgents, forMatchingAgents
      -- * Axioms for agents
    , AxiomSet(..), axTD, systemK, systemS4, systemS5
    , Axioms, axioms, hasAxiom
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort)
import Control.Arrow (second)
import Control.Monad

import Moltap.Util.Util

--------------------------------------------------------------------------------
-- Names & variables
--------------------------------------------------------------------------------

-- | An agent is represented as a string
type Agent = String

--------------------------------------------------------------------------------
-- Agent sets
--------------------------------------------------------------------------------

-- | A set of agents
data Agents
    = Agents        [Agent] -- ^ include these agents (sorted list)
    | ExcludeAgents [Agent] -- ^ exclude these agents, include all others
  deriving (Eq,Ord)

-- | Construct an agent set from a list of agents to include
listAgents :: [Agent] -> Agents
listAgents = Agents . sort

-- | The set of all agents
allAgents :: Agents
allAgents = ExcludeAgents []

-- | Is a set of agents empty?
isNoAgents :: Agents -> Bool
isNoAgents (Agents []) = True
isNoAgents _           = False

-- | Is a set of agents full?
isAllAgents :: Agents -> Bool
isAllAgents (ExcludeAgents []) = True
isAllAgents _                  = False

-- | Give the union of two sets of agents
unionAgents :: Agents -> Agents -> Agents
unionAgents (Agents       xs)  (Agents        ys) = let (_,u,_,_) = iud xs ys in Agents u
unionAgents (Agents       xs)  (ExcludeAgents ys) = let (_,_,_,y) = iud xs ys in ExcludeAgents y
unionAgents (ExcludeAgents xs) (Agents        ys) = let (_,_,x,_) = iud xs ys in ExcludeAgents x
unionAgents (ExcludeAgents xs) (ExcludeAgents ys) = let (i,_,_,_) = iud xs ys in ExcludeAgents i

-- | Give the intersection and difference of two sets of agents
--
-- > intersectAgents a b = (agents in both a and b, agents only in a)
intersectAgents :: Agents -> Agents -> (Agents, Agents)
intersectAgents (Agents        xs) (Agents        ys) = let (i,_,x,_) = iud xs ys in (Agents i, Agents x)
intersectAgents (Agents        xs) (ExcludeAgents ys) = let (i,_,x,_) = iud xs ys in (Agents x, Agents i)
intersectAgents (ExcludeAgents xs) (Agents        ys) = let (_,u,_,y) = iud xs ys in (Agents y, ExcludeAgents u)
intersectAgents (ExcludeAgents xs) (ExcludeAgents ys) = let (_,u,x,_) = iud xs ys in (ExcludeAgents u, Agents x)

--------------------------------------------------------------------------------
-- Collecting values
--------------------------------------------------------------------------------

-- | Collect @b@ values for matching agent sets, split the agent set a if necessary.
--
--   For example:
--
--   > splitMatchingAgents {w,x,y,z} [({w,x,y},A), ({y,z,v},B), ({u},C)]
--   >  == [({w,x},[A]), ({y},[A,B]), ({z},[])]
splitMatchingAgents :: Agents -> [(Agents,b)] -> [(Agents,[b])]
splitMatchingAgents a _ | isNoAgents a = []
splitMatchingAgents a []          = [(a,[])]
splitMatchingAgents a ((a',b):bs) = splitMatchingAgents difference bs
                                 ++ map (second (b:)) (splitMatchingAgents intersection bs)
  where (intersection, difference) = intersectAgents a a'

-- | Collect @c@ values for matching agent sets.
--   Perform an action for each resuling subset.
forMatchingAgents :: MonadPlus m
                  => Agents
                  -> [(Agents, c)]           -- ^ Actions to match
                  -> (Agents -> [c] -> m b)  -- ^ Action to perform for each subset
                  -> m b
forMatchingAgents a bs act = foldr1 mplus . map (uncurry act) . splitMatchingAgents a $ bs

--------------------------------------------------------------------------------
-- Displaying
--------------------------------------------------------------------------------

showAgents :: String -> Agents -> ShowS
showAgents k agents
  | isAllAgents agents = showString k
  | otherwise          = showString k . showChar '_' . shows agents

instance Show Agents where
    showsPrec _ (Agents        [x]) = showString x
    showsPrec _ (Agents        xs)  = showChar '{' . showListWithSep "," showString xs . showChar '}'
    showsPrec _ (ExcludeAgents [])  = showString "*"
    showsPrec _ (ExcludeAgents xs)  = showString "~{" . showListWithSep "," showString xs . showChar '}'

--------------------------------------------------------------------------------
-- Axioms
--------------------------------------------------------------------------------

-- | Additional axioms that can hold
data AxiomSet = AxiomSet { axD, axT, ax4, ax5 :: Bool }

axTD :: AxiomSet -> Bool
axTD ax = axD ax || axT ax

instance Show AxiomSet where
    show (AxiomSet _ True True False) = "S4"
    show (AxiomSet _ True True True ) = "S5"
    show (AxiomSet d t a4 a5) = 'K' : [ 'D' | d && not t ] ++ [ 'T' | t ] ++ [ '4' | a4 ] ++ [ '5' | a5 ]

-- | Well know axiom systems
systemK, systemS4, systemS5 :: AxiomSet
systemK  = AxiomSet False False False False
systemS4 = AxiomSet False True  True  False
systemS5 = AxiomSet False True  True  True

data Axioms = Axioms
    { axAll      :: AxiomSet
    , axSpecific :: Map Agents AxiomSet
    }
  deriving (Show)

axioms :: AxiomSet -> Axioms
axioms a = Axioms a Map.empty

-- | Does an axiom hold for an agent?
hasAxiom :: (AxiomSet -> Bool) -> Agents -> Axioms -> Bool
hasAxiom test a ax = test (Map.findWithDefault (axAll ax) a (axSpecific ax))
