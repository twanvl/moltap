
module Moltap.Prover.Prover
    ( prove
    ) where

import qualified Data.Map as Map

import Moltap.Base.Syntax
import Moltap.Base.Agents
import Moltap.Base.Proof
import Moltap.Base.Model
import Moltap.Prover.TableauState

--------------------------------------------------------------------------------
-- High level interface
--------------------------------------------------------------------------------

-- | Prove a formula or find a counter model
prove :: Program -> Either Proof CounterModel
prove = uncurry proveFormula . evalProgram

-- | Prove a formula or find a counter model
proveFormula :: Axioms -> Formula -> Either Proof CounterModel
proveFormula ax f = case execSSM (emptyTableauState ax) (tableaux f (True :+ emptyHP)) of
            Left  prf -> Left $ simplifyProof prf
            Right tab -> Right $ simplifyModel (|/= f)
                               $ axiomaticClosure (agents f) ax
                               $ mkCounterModel tab

--------------------------------------------------------------------------------
-- Universals and memoization
--------------------------------------------------------------------------------

type TableauState = GenTableauState (Signed HalfProof) Proof
type TableauM     = GenTableauM_    (Signed HalfProof) Proof

exhausted :: TableauM
exhausted = return ()

-- | Add the given signed term to the tableau, and continue reasoning
tableaux :: Formula -> Signed HalfProof -> TableauM
tableaux = tabMemo (\f (s1 :+ p1) (s2 :+ p2) ->
                           if s1 == s2
                           then exhausted
                           else abort $ proofAssum f p1 p2
                   ) tableaux'

--------------------------------------------------------------------------------
-- Recursion over formulas
--------------------------------------------------------------------------------

tableaux' :: Formula -> Signed HalfProof -> TableauM

tableaux' (Truth     t) (s :+ p)
    | s == t                     = abort $ proofTruth s p
    | otherwise                  = exhausted

-- in general, there is nothing we can say about variables
tableaux' (Var       _) _        = exhausted

-- for not: flip the sign
tableaux' (Not       a) (s :+ p) = tableaux a (not s :+ ProofNot s a `hp` p)

-- equivalence
tableaux' (Bin   o a b) (s :+ p)
    -- to proof "a <-> b +", proof "~a or b  and  a or ~b
    | s == signOuter && isEquiv =     do tableaux a (    s :+ ProofBin s o a b `hpL` p)
                                         tableaux b (not s :+ ProofBin s o a b `hpL` p)
                              `split` do tableaux a (not s :+ ProofBin s o a b `hpR` p)
                                         tableaux b (    s :+ ProofBin s o a b `hpR` p)
    -- to proof "a != b +", proof "a or b  and  ~a or ~b"
    | isEquiv                  =      do tableaux a (    s :+ ProofBin s o a b `hpL` p)
                                         tableaux b (    s :+ ProofBin s o a b `hpL` p)
                              `split` do tableaux a (not s :+ ProofBin s o a b `hpR` p)
                                         tableaux b (not s :+ ProofBin s o a b `hpR` p)
    -- to proof "a & b +", we have to proof both
    | s == signOuter           =      tableaux a (signL s :+  ProofBin s o a b `hpL` p)
                              `split` tableaux b (signR s :+  ProofBin s o a b `hpR` p)
    -- to proof "a & b -", we have to proof either one
    | otherwise                =      tableaux a (signL s :+  ProofBin s o a b `hp` p)
                              `par`   tableaux b (signR s :+  ProofBin s o a b `hp` p)
  where (isEquiv,signOuter,signL,signR) = operatorSign o

-- box/diamond
tableaux' f@(Box  b a t) (s :+ p)
    | s == b                   = tabLocalUpIf ax4 a (tableaux f (s :+ Proof4 s a t `hp` hpCut s a f p))
                               $ let sp' = s :+ ProofBox s a t `hp` p in
                                 tabLocalDown a sp' -- it is a goal, skolem
                               $ tableaux t sp'
    | otherwise                = do tabWhenAxiom (\x -> axD x && not (axT x)) a $
                                        -- axiom D : K x -> M x
                                        tableaux (Box (not b) a t) (s :+ ProofD s a t `hp` p)
                                    tabLocalUpIf ax5 a (tableaux f (s :+ Proof5 s a t `hp` hpCut s a f p)) $ do
                                        -- axiom 5? 
                                        tabAddUniv a $ do
                                            -- add universal
                                            -- also always use the proof for the world we end up in
                                            -- if instantiated for agents a' add a "generalization" proof step
                                            a' <- tabGetAgents
                                            _ :+ (box : p') <- tabGetFormula
                                            tableaux t (s :+ (box : p') `hpMerge` (box : ProofGen s a' a t `hp` p))
                                        tabWhenAxiom axT a $
                                            -- axiom T : x -> M x
                                            tableaux t (s :+ ProofT s a t `hp` p)

-- common knowledge
tableaux' f@(Star b a t) (s :+ p)
    | s == b                   = error "Can't prove common knowledge yet"
    | otherwise                = tabLocalUpIf ax5 a (tableaux f (s :+ p))
                               $ do let rec = (tableaux t (s :+ p) >> tabAddUniv a rec)
                                    tabAddUniv a rec
                                    rec

-- ignore notes
tableaux' (Note _ f) sp        = tableaux' f sp


--------------------------------------------------------------------------------
-- Countermodels
--------------------------------------------------------------------------------

type CounterModel = Model Int

-- | Make a counter model representing a tableaux state
mkCounterModel :: TableauState -> CounterModel
mkCounterModel tb = Model
    { modValuation = Map.fromList
                     [ (l,vars)
                     | (l,tab,_) <- graph
                     , let vars = Map.fromList [ (v, not s) | (Var v, (s :+ _)) <- Map.toList $ tabSeen tab ]
                     ]
    , modRelations = Map.fromListWith (Map.unionWith (++))
                     [ (a, Map.singleton l1 [l2])
                     | (l1,_,links) <- graph
                     , (l2, Label a _) <- links
                     ]
    , modRoot      = 0
    }
  where graph = tabGraph tb
