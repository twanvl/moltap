{-# OPTIONS_GHC -w -W #-}

module Moltap.Test where

import Moltap.Base.Model
import Moltap.Base.ModelGraphviz
import Moltap.Base.Syntax
import Moltap.Base.Parser
import Moltap.Base.Proof
import Moltap.Prover.Prover
import Moltap.Util.SimpleJSON
import Moltap.Util.Graphviz
import Moltap.CGI.HTMLOutput ()
import Moltap.Examples

import Control.Monad

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

-- Test that the term indeed doesn't hold in the counter model
testCounter p = case proof p of
                 Right cm | cm |/= f -> True
                 Right cm            -> error $ "counter model is wrong: \n" ++ show cm ++ "\nfor:\n" ++ show p
                 _                   -> error $ "expecting to be false: \n" ++ show p
    where (_,f) = evalProgram p

testProof p = case proof p of
                 Left prf | prf |- f -> True
                 Left prf            -> error $ "proof is not correct: \n" ++ show (traceProof prf)
                 Right _             -> error $ "expecting to be true: \n" ++ show p
    where (_,f) = evalProgram p

-- | True terms should be true, false terms should be false
test = all testProof trueTerms
    && all testCounter falseTerms

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

p = proof . unsafeParse

pm t = case t of
  Left  prf   -> do putStrLn "true"
                    --print prf
                    --print $ runProof prf
                    print $ traceProof prf
  Right model -> do putStrLn "false"
                    graph <- runGraphviz Neato "test-model.png" $ modelToDot model
                    print $ liftM toJSON graph

pp = pm . p

pf n = pp =<< readFile n
