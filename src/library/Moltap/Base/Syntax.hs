{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Moltap.Base.Syntax
    ( -- * Formula syntax tree
      VarName
    , Formula(..), Note(..)
    , Sign, Signed(..)
      -- ** Operators
    , BinOp(..), precedence, operatorSign
      -- ** Top level syntax
    , Program(..)
    , evalProgram, traverseProgram
      -- * Properties
    , agents
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

import Moltap.Base.Agents

--------------------------------------------------------------------------------
-- Names & variables
--------------------------------------------------------------------------------

type VarName = String

--------------------------------------------------------------------------------
-- Formulas
--------------------------------------------------------------------------------

-- | Modal formulas
data Formula
    = Truth   Bool
    | Var     VarName
    | Not     Formula
    | Bin     BinOp Formula Formula -- ^ A binary operator
    | Box     Sign  Agents Formula  -- ^ positive is box, negative is diamond.
    | Star    Sign  Agents Formula  -- ^ common knowledge
    -- Additional syntax
    | Note    Note Formula
  deriving (Eq,Ord)

-- | Unimportant notes
data Note
    = NoteTruth [(String,Bool)] -- ^ in which worlds is this formula true\/false?
  deriving (Eq,Ord)

-- | 'positive' or 'negative'
type Sign = Bool
data Signed a = Sign :+ a
   deriving (Eq,Ord)

infix 3 :+

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

-- | A binary operator that combines two formulas into a larger one
data BinOp
    = And
    | Or
    | Imp
    | Pmi
    | Equiv
    | Differ
  deriving (Eq,Ord)

instance Show BinOp where
    show And    = "&"
    show Or     = "|"
    show Imp    = "->"
    show Pmi    = "<-"
    show Equiv  = "<->"
    show Differ = "<-/->"

-- | The precedence and associativity of an operator
precedence :: BinOp -> (Int, Int, Int)
precedence And = (60,60,60)
precedence Or  = (50,50,50)
precedence Imp = (40,45,40)
precedence _   = (40,45,45)

-- | Information on operators:
--
--   > 1. is it an equivalence operator == or /=
--   > 2. is it a conjunction / positive equivalence
--   > 3. how to flip the sign for the left/right part
operatorSign :: BinOp -> (Bool, Sign, Sign -> Sign, Sign -> Sign)
operatorSign And    = (False, True,  id,  id)
operatorSign Or     = (False, False, id,  id)
operatorSign Imp    = (False, False, not, id)
operatorSign Pmi    = (False, False, id,  not)
operatorSign Equiv  = (True,  True,  undefined, undefined)
operatorSign Differ = (True,  False, undefined, undefined)

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

{-

-- Idea: Compare  x & y  equal to  ~(~x | ~y)

instance Eq Term where
    f == g  =  compare f g == EQ

instance Ord Term where
    compare a b = comp True True a b
      where
        comp s t (Truth   a) (Truth   b) = (s==a) `compare` (t==b)
        comp _ _ (Truth   _) _           = LT
        comp s t (Var     v) (Var     w) = compare s t `mappend` compare v w
        comp _ _ (Var     _) _           = LT
        comp s t (Not     a) b           = comp (not s) t a b
        comp s t a           (Not     b) = comp (not s) t a b
        comp s t (Bin o a b) (Bin p c d) = (s==signOut o) (t==signOut p)
        comp s t (Bin o a b) x           = LT

-}

--------------------------------------------------------------------------------
-- Top level
--------------------------------------------------------------------------------

-- | A program is a formula which can contain extra progamming constructs
data Program
    = Formula Formula
    | Let'    VarName Formula  Program
    | System  Agents  AxiomSet Program

-- | Extract the formula and axioms from a program
evalProgram :: Program -> (Axioms,Formula)
evalProgram = evalProg' systemS5 Map.empty
 where evalProg'  ax env (Formula     f) = (axioms ax, evalFormula env f)
       evalProg'  ax env (Let'    x f p) = evalProg' ax (Map.insert x (evalFormula env f) env) p
       evalProg' _ax env (System  _ a p) = evalProg' a env p

evalFormula :: Map VarName Formula -> Formula -> Formula
evalFormula = traverseFormula (\_ s -> s)

--------------------------------------------------------------------------------
-- Traversing formulas and programs
--------------------------------------------------------------------------------

-- | Traverse a formula bottom up
traverseProgram :: (Formula -> Formula -> Formula) -> Program -> Program
traverseProgram fun = trav Map.empty
 where trav env (Formula     f) = Formula      $ traverseFormula fun env f
       trav env (Let'    x f p) = Let'    x f' $ trav (Map.insert x f' env) p  where f' = traverseFormula fun env f
       trav env (System  l a p) = System  l a  $ trav env p

-- | Traverse a formula bottom up, the function will get the new formula and the simplified formula
traverseFormula :: (Formula -> Formula -> Formula) -> Map VarName Formula -> Formula -> Formula
traverseFormula f env = fst . trav
 where trav x@(Truth    _) = f' x x
       trav x@(Var      v) = f' x $ Map.findWithDefault x v env
       trav   (Not      a) = f' (Not  a1)      (Not  a2)       where (a1,a2) = trav a
       trav   (Bin  o a b) = f' (Bin  o a1 b1) (Bin  o a2 b2)  where (a1,a2) = trav a; (b1,b2) = trav b
       trav   (Box  s a b) = f' (Box  s a b1)  (Box  s a b2)   where (b1,b2) = trav b
       trav   (Star s a b) = f' (Star s a b1)  (Star s a b2)   where (b1,b2) = trav b
       trav   (Note   n a) = f' (Note n a1)    (Note n a2)     where (a1,a2) = trav a
       f' f1 f2 = (f f1 f2, f2)

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | The agents mentioned in a formula
agents :: Formula -> Set Agents
agents (Truth    _) = Set.empty
agents (Var      _) = Set.empty
agents (Not      t) = agents t
agents (Bin  _ a b) = agents a `Set.union` agents b
agents (Box  _ a t) = Set.insert a (agents t)
agents (Star _ a t) = Set.insert a (agents t)
agents (Note   _ a) = agents a

--------------------------------------------------------------------------------
-- Displaying
--------------------------------------------------------------------------------

instance Show a => Show (Signed a) where
    showsPrec p (False :+ a) = showsPrec p a . showString "-"
    showsPrec p (True  :+ a) = showsPrec p a . showString "+"

instance Show Formula where
    showsPrec _ (Truth    t) = showString (if t then "true" else "false")
    showsPrec _ (Not      a) = showString "~" . showsPrec 70 a
    showsPrec _ (Var      v) = showString v
    showsPrec p (Bin  o a b) = showParen (p > outer) $ showsPrec left a . showString " " . shows o . showString " "  . showsPrec right b
                               where (outer,left,right) = precedence o
    showsPrec p (Box  s a b) = showParen (p > 70) $ showAgents (if s then "K" else "M")   a . showString " " . showsPrec 70 b
    showsPrec p (Star s a b) = showParen (p > 70) $ showAgents (if s then "K*" else "M*") a . showString " " . showsPrec 70 b
    showsPrec p (Note   _ t) = showsPrec p t

instance Show Program where
    showsPrec p (Formula     f) = showsPrec p f
    showsPrec _ (Let'    v a p) = showString "let " . showString v . showString " = " . showsPrec 10 a
                                . showString "; " . showsPrec 0 p
    showsPrec _ (System  l a p) = showString "system " . shows a . (if isAllAgents l then id else showString " for " . shows a)
                                . showString "; " . showsPrec 0 p
