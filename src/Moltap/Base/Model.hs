{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- | This module defines Kripke models,
--   and the evaluation of terms inside these models.
module Moltap.Base.Model
    ( Model(..), modWorlds, modIsWorld
    -- * Evaluation
    , Models(..), annotate
    -- * Transformations
    , toIntKeys
    , simplifyModel
    , axiomaticClosure
    , transSymClosure, addReflexive
    ) where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad
import Control.Arrow (first,(***))

import Moltap.Util.Util
import Moltap.Base.Syntax
import Moltap.Base.Agents

--------------------------------------------------------------------------------
-- Counter models
--------------------------------------------------------------------------------

-- | A Kripke model, the worlds are of the type @world@
data Model world = Model
    { -- | A valuation, in what world are what variables true\/false?
      modValuation :: Map world (Map VarName Bool)
      -- | Accessability relations for each agent
    , modRelations :: Map Agents (Relation world)
      -- | The root world of the model, terms are evalutated here
    , modRoot      :: world 
    }
  deriving (Show)

-- | All worlds in a Kripke model
modWorlds :: Model w -> [w]
modWorlds = Map.keys . modValuation

-- | Does the given world actually exist?
modIsWorld :: Ord w => w -> Model w -> Bool
modIsWorld world mod = world `Map.member` modValuation mod

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Evaluating terms in a model.
--
--   * @m |= t@ evaluates @t@ in the root world of the model.
--
--   * @(m,w) |= t@ evaluates @t@ in the world @w@ of the model.
class Models a where
    -- | @m |= term@ should be read as \"m models term\".
    --   Does the term evaluate to @True@ in the model?
    (|=)  :: a -> Formula -> Bool
    -- | Does not model
    (|/=) :: a -> Formula -> Bool
    a |/= b = not (a |= b)

instance Ord w => Models (Model w) where
    m |= t = (m, modRoot m) |= t

instance Ord w => Models (Model w,w) where
    (_,_) |= Truth    t = t
    (m,w) |= Var      v = fromMaybe False $ Map.lookup v =<< Map.lookup w (modValuation m)
    (m,w) |= Not      f = ((m,w) |/= f)
    (m,w) |= Bin  o f g = ((m,w) |= f) ?? ((m,w) |= g) where (??) = evalOperator o
    (m,w) |= Box  s a t = (m, if s then All ws else Any ws) |= t
        where ws = Set.fromList
                   [ w'
                   | Just rel <- return $ Map.lookup a (modRelations m)
                   , w' <- Map.findWithDefault [] w rel
                   , modIsWorld w' m
                   ]
    (m,w) |= Star s a t = (if s then and else or)
                              [ (m,w') |= t
                              | Just rel <- return $ Map.lookup a (modRelations m)
                              , w' <- Map.findWithDefault [] w (transClose True [w] rel)
                              , modIsWorld w' m -- only actually existing worlds
                              ]
    (m,w) |= Note _ f   = (m,w) |= f

-- | A conjunction or disjunction of worlds
data Many w = All (Set w) | Any (Set w)

instance Ord w => Models (Model w, Many w) where
    (m, All ws) |= f  =  and [ (m,w) |= f | w <- Set.toList ws ]
    (m, Any ws) |= f  =  or  [ (m,w) |= f | w <- Set.toList ws ]


-- | Binary operators
evalOperator :: BinOp -> (Bool -> Bool -> Bool)
evalOperator And    = (&&)
evalOperator Or     = (||)
evalOperator Imp    = (<=)
evalOperator Pmi    = (>=)
evalOperator Equiv  = (==)
evalOperator Differ = (/=)

--------------------------------------------------------------------------------
-- Annotate terms with truth values
--------------------------------------------------------------------------------

-- | Annotate each subformula of a 'Program' with a 'Note' that specifies in what worlds that formula is true.
annotate :: Model Int -> Program -> Program
annotate m = traverseProgram ann
  where ann f' (Note _ _) = f'
        ann f' simple     = Note (truths simple) f'
        truths f = NoteTruth [ (show i, (m,i) |= f) | i <- modWorlds m ]

--------------------------------------------------------------------------------
-- Integer keys
--------------------------------------------------------------------------------

-- | Use integer keys for a model.
--   This allows for faster computation, and for correct graphviz conversion.
toIntKeys :: Ord w => Model w -> Model Int
toIntKeys mod = Model { modValuation = Map.fromAscList . map (first newKey) . Map.toAscList $ modValuation mod
                      , modRelations = Map.map (Map.fromAscList . map (newKey *** map newKey) . Map.toAscList) $ modRelations mod
                      , modRoot      = newKey $ modRoot mod
                      }
  where mapping = Map.fromList $ zip (modWorlds mod) [0..]
        newKey  = (mapping Map.!)

--------------------------------------------------------------------------------
-- Closure
--------------------------------------------------------------------------------

-- | Take the reflexive, transitive, symmetric closure of a counter model,
--   but only as needed by the axioms.
axiomaticClosure :: Ord w => Set Agents -> Axioms -> Model w -> Model w
axiomaticClosure agents ax mod = mod''
  where mod'  = addReflexive (Set.filter (\a -> hasAxiom axTD a ax) agents) mod
        mod'' = mod' { modRelations = Map.mapWithKey closeRel (modRelations mod') }
        closeRel a rel = axiomaticClosureRel (\t -> hasAxiom t a ax) (modWorlds mod') rel

-- | Take the reflexive, transitive, symmetric closure of a relation,
--   but only as needed by the axioms.
axiomaticClosureRel :: Ord w => ((AxiomSet -> Bool) -> Bool) -> [w] -> Relation w -> Relation w
axiomaticClosureRel ax worlds
   | ax axT && ax ax4 && ax ax5 = transClose (ax axT) worlds . symmClose
   |           ax ax4 && ax ax5 = transClose (ax axT) worlds . euclideanClose
   |                     ax ax5 = euclideanClose
   |           ax ax4           = transClose (ax axT) worlds
   | ax axT                     = reflClose worlds
   | otherwise                  = id

-- | Take the reflexive, transitive, symmetric closure of a counter model
transSymClosure :: Ord w => Model w -> Model w
transSymClosure mod = mod { modRelations = Map.map (transClose True (modWorlds mod) . symmClose) (modRelations mod) }

-- | Add reflexive arrows for the given set of agents,
--   only add arrows to worlds without outgoing ones.
addReflexive :: Ord w => Set Agents -> Model w -> Model w
addReflexive agents mod = mod { modRelations = Map.unionWith (Map.unionWith const) (modRelations mod) newRelations }
  where newRelations = Map.fromAscList [ (a,reflexiveRel) | a <- Set.toAscList agents ]
        reflexiveRel = Map.fromAscList [ (w,[w]) | w <- modWorlds mod ]

--------------------------------------------------------------------------------
-- Simplification
--------------------------------------------------------------------------------

-- | Simplfy a model by merging worlds.
--   Ensures that the model still satisfies a predicate
simplifyModel :: Ord w => (Model w -> Bool) -> Model w -> Model w
simplifyModel pred mod = trySimplify [ (a,b) | (a:bs) <- tails (modWorlds mod), b <- bs ]
  where
    -- Try all possible simplifications
    trySimplify [] = mod
    trySimplify ((a,b):abs) = case tryMergeWorlds a b of
                                Just mod' -> simplifyModel pred mod'
                                Nothing   -> trySimplify abs
    -- Try to merge worlds a and b
    tryMergeWorlds a b = do
          vars' <- unifyVars (modValuation mod Map.! a) (modValuation mod Map.! b)
          let mod' = mod { modValuation = Map.insert a vars' $ Map.delete b $ modValuation mod
                         , modRoot = if modRoot mod == b then a else modRoot mod
                         }
          guard (pred mod') -- it is still a valid model
          return mod'

-- | Unify variable sets.
--   Sets can be unified if they contain the same values for all variables they share
unifyVars :: Map VarName Bool -> Map VarName Bool -> Maybe (Map VarName Bool)
unifyVars a b
 | Map.fold (&&) True (Map.intersectionWith (==) a b)  =  Just $ Map.union a b
 | otherwise                                           =  fail "variable sets don't unify"
