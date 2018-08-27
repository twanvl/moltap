--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Examples
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Example terms for testing.
--
--------------------------------------------------------------------------------

module Moltap.Examples where

import Moltap.Base.Syntax
import Moltap.Base.Parser

--------------------------------------------------------------------------------
-- Example terms
--------------------------------------------------------------------------------

-- | Formulas that are true in S5
trueTerms :: [Program]
trueTerms = map unsafeParse
    -- simple
    [ "x | ~x"
    , "true"
    , "K1 (x | ~x)"
    , "M1 x | M1 ~x"
    , "M1 x <-> ~K1 ~x"
    , "(x & y) <-> ~(~x | ~y)"
    , "(x -> y) <-> (~x | y)"
    -- hard
    , "K_1 M_1 M_3 K_3 K_2 K_1 a -> a"
    , "~(K_1(a | ~b) & M_1 ~a & M_1(~a & b))"
    , "~M_1(~a & a)"
    , "K_1 M_1 M_3 K_3 K_2 K_3 a -> M_1 M_3 a"
    , "~(K_1(a | b) & M_1 ~a & M_1 K_1 M_3 ~b & (M_1 K_2 M_1(~a & ~b) | M_1(~a & ~b) | K_1 a))"
    , "~(K_1 M_1 M_3 K_3 K_2 K_3 a & K_1 M_3 M_2 K_3 M_2 ~a)"
    , "~(K_1 M_1 M_3 K_3 K_2 K_3 a & K_1 M_3 K_3 K_2 (~a | x) & K_1 ~x)"
    , "K_1 (a -> b) -> ( K_1 a -> K_1 b)"
    , "K_1 a -> a"
    , "K_1 a -> K_1 K_1 a"
    , "M_1 a -> K_1 M_1 a"
    , "M_1 (K_1 p | M_2 K_2 K_1 q) -> K_1 (~q -> p)"
    , "((p & ~K_1 p) -> ~p) | ((p & ~K_1 p) -> ~((p & ~K_1 p) -> ~K_1((p & ~K_1 p) -> p )))"
    , "(K_1 (a | b) & (c -> K_1 ~b) & (K_1 a -> d)) -> (c -> d)"
    , "(K_1(g = K_1 p) & M_1 g) -> (g & p)"
    , "(K_1(g = K_1 p) & M_1 ~g) -> ~g"
    , "(K_1 p & K_1((a & p) -> b)) -> K_1 (a -> b)"
    -- test of proofs
    , "M1 M2 K2 K1 a -> a"
    , "system K5; M1 M2 K2 a -> M1 M2 a"
    , "K1 M1 a & K1 x -> M1 x"
    , "system K; K1 x & K1 ~x -> K1 y"
    , "x -> K1 (M1 x | y)"
    , "K x&z -> K (K (x | y) & x | y)"
    -- other systems
    , "system K;  K1 x & K1 y -> K1 (x & y)"
    , "system K;  K1 x & K1 (x -> y) -> K1 y"
    , "system KD; K1 x -> M1 x"
    , "system KT; K1 x -> M2 x"
    , "system KT; K1 x -> x"
    , "system KT; x -> M1 x"
    , "system K4; K1 a -> K1 K1 a"
    , "system K5; M1 a -> K1 M1 a"
    , "system K5T; M1 K1 a -> a"
    -- group knowledge
    , "K{1,2} x -> K1 x"
    , "K1 x <- K{1,2} x"
    , "K x -> K_john x"
    , "M_bob a -> M a"
    --, "K{1,2} x = K1 x & K2 x"
    ]

-- | Formulas that are false in S5
falseTerms :: [Program]
falseTerms = map unsafeParse
    -- simple
    [ "x | x"
    , "x & ~x"
    , "false"
    , "K1 x | K1 ~x"
    -- harder
    , "(K_1 p & K_1 ((a & p) -> b)) -> (M_1 ~b & K_1 (a -> b))"
    , "K_1 M_1 M_3 K_3 K_2 K_3 a -> b"
    , "~(K_1 (a | ~b) & M_1 ~a & M_1 b)"
    , "K_1 M_1 M_3 K_3 K_2 M_3 a -> K_1 K_3 a"
    , "~(K_1 (a | b) & M_1 ~a & (M_1 (~a & ~b) | M_1 a))"
    , "~(K_1 M_1 M_3 K_3 K_2 K_3 a & K_1 M_3 K_3 K_2 (~a | x) & ~x)"
    , "((p & ~K_1 p) -> p) & ((p & ~K_1 p) -> ~((p & ~K_1 p) -> K_1((p & ~K_1 p) -> p )))"
    , "~(K_1 M_3 (M_1 a | K_4 b) & K_3 (K_1 K_3 ~a & M_3 K_4 K_3 M_6 ~b))"
    , show threeHats3
    -- other systems
    , "system K;    K1 x & K2 y -> K1 (x & y)"
    , "system K;    M1 True"
    , "system KD45; K1 x -> x"
    , "system K45;  K1 x -> M1 x"
    , "system KD45; K1 x -> M2 x"
    , "system K5DT; K1 a -> K1 K1 a"
    , "system K4DT; M1 a -> K1 M1 a"
    , "system KD45; M1 K1 a -> a" -- needs T5
    , "system KT4;  M1 K1 a -> a"
    ]

threeHats :: Program
threeHats = unsafeParse $ unlines
    [ "# There are only two blue hats"
    , "let two_hats = (b1&b2 -> ~b3) & (b3&b1 -> ~b2) & (b2&b3 -> ~b1);"
    , ""
    , "# Each agent can see the colors of the other agents' hats"
    , "let see_other = "
    , "   (K2 b1 | K2 ~b1) & (K3 b1 | K3 ~b1) &"
    , "   (K3 b2 | K3 ~b2) & (K1 b2 | K1 ~b2) &"
    , "   (K1 b3 | K1 ~b3) & (K2 b3 | K2 ~b3);"
    , ""
    , "# All agents know the rules"
    , "K* (two_hats & see_other)"
    , ""
    , "# What if there are two blue hats?"
    , "& b1 & b2"
    , ""
    , "# Does any agent know the color of his own hat?"
    , "-> K1 b1 | K1 ~b1 | K2 b2 | K2 ~b2 | K3 b3 | K3 ~b3"
    ]

threeHats2 :: Program
threeHats2 = unsafeParse $ unlines
    [ "# There are only two blue hats"
    , "let two_hats = (b1&b2 -> ~b3) & (b3&b1 -> ~b2) & (b2&b3 -> ~b1);"
    , ""
    , "# Each agent can see the colors of the other agents' hats"
    , "let see_other = "
    , "   (K2 b1 | K2 ~b1) & (K3 b1 | K3 ~b1) &"
    , "   (K3 b2 | K3 ~b2) & (K1 b2 | K1 ~b2) &"
    , "   (K1 b3 | K1 ~b3) & (K2 b3 | K2 ~b3);"
    , ""
    , "# All agents know the rules"
    , "K* (two_hats & see_other)"
    , ""
    , "# There are two blue hats"
    , "& (b1 & b2)"
    , ""
    , "# Does any agent know the color of his own hat?"
    , "-> K1 b1 | K1 ~b1 | K2 b2 | K2 ~b2 | K3 b3 | K3 ~b3"
    ]

threeHats3 :: Program
threeHats3 = unsafeParse $ unlines
    [ "# There are only two blue hats"
    , "let two_hats = (b1&b2 -> ~b3) & (b3&b1 -> ~b2) & (b2&b3 -> ~b1);"
    , ""
    , "# Each agent can see the colors of the other agents' hats"
    , "let see_other = "
    , "   (K2 b1 | K2 ~b1) & (K3 b1 | K3 ~b1) &"
    , "   (K3 b2 | K3 ~b2) & (K1 b2 | K1 ~b2) &"
    , "   (K1 b3 | K1 ~b3) & (K2 b3 | K2 ~b3);"
    , ""
    , "# All agents know the rules"
    , "K* (two_hats & see_other)"
    , ""
    , "# There is at most one blue hat"
    , "& ~(b1 & b2) & ~(b2 & b3) & ~(b3 & b1)"
    , ""
    , "# Does any agent know the color of his own hat?"
    , "-> K1 b1 | K1 ~b1 | K2 b2 | K2 ~b2 | K3 b3 | K3 ~b3"
    ]
