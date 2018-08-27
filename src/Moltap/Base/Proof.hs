{-# LANGUAGE TypeSynonymInstances, PatternGuards #-}

module Moltap.Base.Proof
   ( -- * Proofs
     Proof
   , isComplete
   , Sequent, WithSequent(..)
   , runProof, traceProof, (|-)
   , simplifyProof
     -- * Proof trees
   , ProofTree(..)
     -- * Proof steps
   , ProofStep(..)
   , proofStep, principal
     -- * Half proofs
   , HalfProof
   , emptyHP, hp, hpL, hpR, hpMerge, hpCut
   , proofTruth, proofAssum
   ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.Error
import Data.Monoid
import Data.List
import Data.Maybe

import Moltap.Base.Syntax
import Moltap.Base.Agents
import Moltap.Util.Util

import Debug.Trace

--------------------------------------------------------------------------------
-- Proof trees
--------------------------------------------------------------------------------

-- | A proof tree consists of a (possibly branching) step or it is an invalid tree
data ProofTree a
    = ProofStep a [ProofTree a]
    | NoProof
  deriving ()

type Proof = ProofTree ProofStep

instance Show a => Show (ProofTree a) where
    show = unlines . reverse . proofTreeLines

instance Functor ProofTree where
    fmap f (ProofStep a ps) = ProofStep (f a) (map (fmap f) ps)
    fmap _ NoProof          = NoProof

-- | Show a proof tree as a list of lines
proofTreeLines :: Show a => ProofTree a -> [String]
proofTreeLines (ProofStep s [p]) = lines (show s) ++ proofTreeLines p
proofTreeLines (ProofStep s ps)  = lines (show s) ++ concat (intersperse ["AND"] [map ("  "++) (proofTreeLines p) | p <- ps])
proofTreeLines NoProof           = ["NO PROOF"]

-- | Is a proof complete?
isComplete :: Proof -> Bool
isComplete (ProofStep _ ps) = all isComplete ps
isComplete NoProof          = False

--------------------------------------------------------------------------------
-- Proof steps
--------------------------------------------------------------------------------

-- | A single step in a proof
data ProofStep
    = ProofTruth  Sign                        -- ^ true/false axioms
    | ProofAssum       Formula                -- ^ assumption axiom
    | ProofCut         Formula                -- ^ cut rule
    | ProofNot    Sign Formula                -- ^ negation
    | ProofBin    Sign BinOp Formula Formula  -- ^ binary operator
    | ProofBox    Sign Agents Formula         -- ^ necessitation: box all formulas, the given formula is boxed 'positively'
    | ProofD      Sign Agents Formula         -- ^ @K a -> M a@
    | ProofT      Sign Agents Formula         -- ^ @a   -> M a@
    | Proof4      Sign Agents Formula         -- ^ @K a -> K K a@
    | Proof5      Sign Agents Formula         -- ^ @M a -> K M a@
    | ProofGen    Sign Agents Agents Formula  -- ^ @M{a} x -> M{a,b} x@  (@K {a,b} x -> K a x@)
    -- for constructing proofs
    | ProofFromCut                            -- ^ Marking the \'inner\' branch of a cut for a 4 or 5 step
  deriving (Eq, Ord)

instance Show ProofStep where
    showsPrec _ (ProofTruth      s      ) = showString "Truth" . showSign s
    showsPrec _ (ProofAssum        f    ) = showString "Assum: " . shows f
    showsPrec _ (ProofCut          f    ) = showString "Cut: " . shows f
    showsPrec _ (ProofNot        s f    ) = showString "Not"   . showSign s . showString ": " . shows f
    showsPrec _ (ProofBin        s o a b) = showString "Bin" . shows o . showSign s . showString ": " . shows a . showString "  " . shows o . showString "  " . shows b
    showsPrec _ (ProofBox        s a f  ) = showAgents "Box" a . showSign s . showString ": " . shows f
    showsPrec _ (ProofD          s a f  ) = showAgents "D"   a . showSign s . showString ": " . shows f
    showsPrec _ (ProofT          s a f  ) = showAgents "T"   a . showSign s . showString ": " . shows f
    showsPrec _ (Proof4          s a f  ) = showAgents "4"   a . showSign s . showString ": " . shows f
    showsPrec _ (Proof5          s a f  ) = showAgents "5"   a . showSign s . showString ": " . shows f
    showsPrec _ (ProofGen        s a b f) = showAgents "Gen" a . showAgents "" b . showSign s . showString ": " . shows f
    showsPrec _ (ProofFromCut           ) = showString "<from cut>"

showSign :: Sign -> ShowS
showSign True  = showString "+"
showSign False = showString "-"

isBox :: ProofStep -> Bool
isBox (ProofBox  _ _ _) = True
isBox _                 = False

--------------------------------------------------------------------------------
-- Evaluating proofs
--------------------------------------------------------------------------------

type Sequent = Set (Signed Formula)
-- | After a proof step a sequent is proven
data WithSequent = (:~) { wsStep :: ProofStep, wsSequent :: Sequent }

instance Show WithSequent where
    show (step :~ sequent) = showSequent sequent "\n---------------------------------- " ++ show step
      where showSequent s = showListWithSep ", " shows [ f | False :+ f <- Set.toList s ]
                          . showString " |- "
                          . showListWithSep ", " shows [ f | True  :+ f <- Set.toList s ]

-- | Run a proof
runProof :: Proof -> Sequent
runProof = getSequent . traceProof

-- | Run a proof, keeping intermediate sequents
traceProof :: Proof -> ProofTree WithSequent
traceProof (ProofStep s ps) = ProofStep (s :~ proofStep s (map getSeq ps')) ps'
                              where ps' = map traceProof ps
                                    getSeq (ProofStep ws _) = wsSequent ws
                                    getSeq _                = error "no proof"
traceProof NoProof          = NoProof

-- | Get the resulting sequent of an annotated proof tree
getSequent :: ProofTree WithSequent -> Sequent
getSequent (ProofStep ws _) = wsSequent ws
getSequent _                = error "no proof"

-- | Test if a proof is a proof of a given formula
(|-) :: Proof -> Formula -> Bool
p |- f  =  runProof p == Set.singleton (True :+ f)


-- | Run a single step from a proof
proofStep :: ProofStep -> [Sequent] -> Sequent
proofStep (ProofBox  s a f) [g] = Set.insert (s :+ Box s a f) $ boxAll a $ Set.delete (s:+f) g
proofStep s                 gs  = Set.unions (ys : zipWithSame Set.difference gs xs)
                                  where (xs,ys) = principal s

boxAll :: Agents -> Sequent -> Sequent
boxAll a = Set.map conv
    where conv (s' :+ f') = s' :+ Box (not s') a f'


-- | Principal formulas of a proof step
principal :: ProofStep -> ([Sequent], Sequent)
principal (ProofTruth   s      ) = []                           |-->  [s :+ Truth s]
principal (ProofAssum     f    ) = []                           |-->  [True :+ f, False :+ f]
principal (ProofCut       f    ) = [[False :+ f], [True :+ f]]  |-->  []
principal (ProofNot     s f    ) = [[not s :+ f]]               |-->  [s :+ Not f]
principal (ProofBin     s o a b) = antecedent                   |-->  [s :+ Bin o a b]
    where antecedent = case (isEquiv, signOuter==s) of
                          (False,False) -> [[signL s :+ a,   signR s :+ b]]
                          (False,True ) -> [[signL s :+ a], [signR s :+ b]]
                          (True, False) -> [[s :+ a, s :+ b], [not s :+ a, not s :+ b]]
                          (True, True ) -> [[s :+ a, not s :+ b], [not s :+ a, s :+ b]]
          (isEquiv,signOuter,signL,signR) = operatorSign o
principal (ProofD       s a f  ) = [[s :+ Box s a f]]           |-->  [s :+ Box (not s) a f]
principal (ProofT       s a f  ) = [[s :+ f]]                   |-->  [s :+ Box (not s) a f]
principal (Proof4       s a f  ) = [[s :+ Box s a f]]           |-->  [s :+ Box s a (Box s a f)]
principal (Proof5       s a f  ) = [[s :+ Box (not s) a f]]     |-->  [s :+ Box s a (Box (not s) a f)]
principal (ProofGen     s a b f) = [[s :+ Box (not s) a f]]     |-->  [s :+ Box (not s) (unionAgents a b) f]
principal (ProofFromCut)         = [[]] |--> []
principal s                      = error $ "Invalid proof step: " ++ show s

(|-->) :: [[Signed Formula]] -> [Signed Formula] -> ([Sequent], Sequent)
xs |--> y = (map Set.fromList xs, Set.fromList y)

--------------------------------------------------------------------------------
-- Half proofs
--------------------------------------------------------------------------------

-- | A half proof is a trace of the proof steps used to reach an axiom
type HalfProof = [HalfProofStep]

data HalfProofStep
    = HP   ProofStep
    | HP_L ProofStep
    | HP_R ProofStep
    | HP_Cut   Sign Agents Formula HalfProof
    | HP_Merge HalfProof
  deriving (Eq,Show)

-- | The empty half proof
emptyHP :: HalfProof
emptyHP = []

-- | Extending half proofs with a single step
hp, hpL, hpR :: ProofStep -> HalfProof -> HalfProof
hp s p          = HP s  : p

hpL s = (HP_L s :)
hpR s = (HP_R s :)

-- | Merge two half proofs
hpMerge :: HalfProof -> HalfProof -> HalfProof
hpMerge a b = HP_Merge a : b

-- | Cut the half proof unto the next box step
hpCut :: Sign -> Agents -> Formula -> HalfProof -> HalfProof
hpCut b a f = hpCut' []
    where hpCut' cut (s : xs) 
            | isStop s   = HP_Cut b a f (reverse (HP ProofFromCut:s:cut)) : xs
            | otherwise  = hpCut' (s:cut) xs
          hpCut' _   []  = error "hpCut: Expected box step"
          isStop (HP s) = isBox s
          isStop _      = False

infixr 9 `hp`, `hpL`, `hpR`, `hpCut`

----------------------------------------------------------------
-- Half proofs to proofs
----------------------------------------------------------------

-- | The truth axiom
proofTruth :: Sign -> HalfProof -> Proof
proofTruth s = unfoldHP1 (ProofStep (ProofTruth s) [])

-- | The assumption axiom
proofAssum :: Formula -> HalfProof -> HalfProof -> Proof
proofAssum f = unfoldHP2 (ProofStep (ProofAssum f) [])

-- | Convert a HalfProof to a full proof
unfoldHP1 :: Proof -> HalfProof -> Proof
unfoldHP1 p []              = p
unfoldHP1 p (HP    step:xs) = unfoldHP1 (ProofStep step [p]) xs
unfoldHP1 p (HP_L  step:xs) = unfoldHP1 (ProofStep step [p, NoProof]) xs
unfoldHP1 p (HP_R  step:xs) = unfoldHP1 (ProofStep step [NoProof, p]) xs
unfoldHP1 p (HP_Cut s a f h:xs) = unfoldHP1 (ProofStep (ProofCut (Box s a f)) (if s then [inner,p] else [p,inner]) ) xs
           where inner = unfoldHP1 (ProofStep (ProofAssum f) []) h
unfoldHP1 p (HP_Merge xs:ys) = mergeProof False (unfoldHP1 p xs) (unfoldHP1 p ys)

-- | Convert two HalfProof to a full proof
unfoldHP2 :: Proof -> HalfProof -> HalfProof -> Proof
unfoldHP2 p a b = --trace ("MERGE: " ++ show a ++ "\nWITH:  " ++ show b) $
                  --trace ("MERGE:\n" ++ show (unfoldHP1 p a)) $
                  --trace ("WITH:\n" ++ show (unfoldHP1 p b)) $
                  --trace ("GIVING:\n" ++ show (mergeProof False (unfoldHP1 p a) (unfoldHP1 p b))) $
                  mergeProof False (unfoldHP1 p a) (unfoldHP1 p b)

--------------------------------------------------------------------------------
-- Merging proofs
--------------------------------------------------------------------------------

instance Monoid Proof where
    mempty  = NoProof
    mappend = mergeProof True

-- | Merge two proofs
--
--   the boolean indicates whether to take the most defined or least defined proof
mergeProof :: Bool -> Proof -> Proof -> Proof
mergeProof True  a                   NoProof            =  a
mergeProof False _                   NoProof            =  NoProof
mergeProof True  NoProof             b                  =  b
mergeProof False NoProof             _                  =  NoProof
mergeProof True  a                   _ | isComplete a   =  a
mergeProof True  _                   b | isComplete b   =  b
mergeProof False _        b@(ProofStep ProofFromCut _)  =  b
mergeProof False a@(ProofStep ProofFromCut _)       _   =  a
mergeProof most a@(ProofStep s ps)  b@(ProofStep t qs) = case stepOrder (s,length ps) (t,length qs) of
    S_Unify u -> ProofStep u (zipWithSame (mergeProof most) ps qs)
    S_First   -> ProofStep s [ mergeProof most p b | p <- ps ]
    S_Second  -> ProofStep t [ mergeProof most a q | q <- qs ]
    S_Error   -> error $ "Can not merge proof steps: " ++ show s ++ " with " ++ show t

data StepOrder
    = S_Unify ProofStep  -- ^ The two steps are unified into a single step
    | S_First            -- ^ The first step should be done first
    | S_Second
    | S_Error            -- ^ The steps are incompatible

-- | Should the first proof step be done before the second?
--   The integers are the number of arguments of the steps
stepOrder :: (ProofStep, Int) -> (ProofStep, Int) -> StepOrder
stepOrder (s,n) (t,m)
    | n == m && s == t                            =  S_Unify s -- it is the same step
stepOrder (s,n) (t,m)
    | (isBox s || n == 0) && (isBox t || m == 0)  =  S_Error   -- Can't merge different box steps or axioms with each other
    | n == 0                                      =  S_Second  -- do axioms last
    | m == 0                                      =  S_First
    | isBox s                                     =  S_Second  -- do boxes last (but before axioms)
    | isBox t                                     =  S_First
    | n > m                                       =  S_Second  -- prefer to split later
    | n < m                                       =  S_First
    | s < t                                       =  S_Second  -- pick an arbitrary order,
    | otherwise                                   =  S_First   --  as long as it is consisten

--------------------------------------------------------------------------------
-- Simplifying proofs
--------------------------------------------------------------------------------

-- | Simplify a proof
simplifyProof :: Proof -> Proof
simplifyProof = fmap wsStep . simplify' . traceProof

-- | Simplify a proof tree
simplify' :: ProofTree WithSequent -> ProofTree WithSequent
simplify' (ProofStep s ps) = let step' = ProofStep s (map simplify' ps) in
                             case simplifyStep step' of
                                Just step'' -> simplify' step''
                                Nothing     -> step'
simplify' NoProof = NoProof

-- | A step in the simplification, return Nothing if nothing has changed, and Just p' for a simpler proof
simplifyStep :: ProofTree WithSequent -> Maybe (ProofTree WithSequent)
simplifyStep (ProofStep _ [])
    -- can't simplify axioms any further
    = Nothing
simplifyStep (ProofStep (_s :~ a) [p@(ProofStep (_ :~ b) _)]) | a == b
    -- the step _s doesn't do anything, skip it
    = Just p
simplifyStep (ProofStep (_ :~ s) _) | Just f <- isAssumAxiom s
    -- matching the assumption axiom
    = Just $ traceProof $ ProofStep (ProofAssum f) []
simplifyStep (ProofStep (ProofCut _ :~ _) [ProofStep (ProofAssum _ :~ _) [], b] )
    -- cut with an axiom on the left
    = Just b
simplifyStep (ProofStep (ProofCut _ :~ _) [a,ProofStep (ProofAssum _ :~ _) []] )
    -- cut with an axiom on the right
    = Just a
simplifyStep _ = Nothing

isAssumAxiom :: Sequent -> Maybe Formula
isAssumAxiom sequent = listToMaybe [ f | s :+ f <- Set.toList sequent, (not s :+ f) `Set.member` sequent ]
