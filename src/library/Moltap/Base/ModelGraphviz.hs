{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Util.Util
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Converting 'Model's to graphviz graphs
--
--------------------------------------------------------------------------------

module Moltap.Base.ModelGraphviz
    ( modelToDot
    ) where

import Data.List
import qualified Data.Map as Map

import Moltap.Util.Util
import Moltap.Base.Agents
import Moltap.Base.Model

--------------------------------------------------------------------------------
-- Edge direction
--------------------------------------------------------------------------------

type Node = Int

data Dir = Forward | Back | Both | Self

instance Show Dir where
    show Forward = "dir=forward"
    show Back    = "dir=back"
    show Both    = "dir=both"
    show Self    = "len=1"

plusDir :: Dir -> Dir -> Dir
plusDir Forward Forward = Forward
plusDir Back    Back    = Back
plusDir Self    Self    = Self
plusDir _       _       = Both

-- | An edge between two nodes, for the given agents and only in the given direction
type Edge = ((Node,Node),(Dir,Agents))

mkEdge :: Node -> Node -> Agents -> Edge
mkEdge i j a
  | i == j    = ((i,j),(Self,   a))
  | i <= j    = ((i,j),(Forward,a))
  | otherwise = ((j,i),(Back,   a))

plusEdge :: (Dir,Agents) -> (Dir,Agents) -> (Dir,Agents)
plusEdge (d,a) (e,b) = (d `plusDir` e, a `unionAgents` b)
  -- NOTE: this is wrong if for different agents we have different edge directions

--------------------------------------------------------------------------------
-- Model -> GraphViz
--------------------------------------------------------------------------------

-- | Convert a model to a GraphViz graph
modelToDot :: Model Int -> String
modelToDot model
    = concat [ "  " ++ show w ++ "[label=" ++ showVals (Map.toList vs)
               ++ ",shape=" ++ (if w == modRoot model then "doublecircle" else "circle")
               ++ ",URL=" ++ show w
               ++ "];\n"
             | (w,vs) <- Map.toList (modValuation model)
             ]
   ++ "  edge[len=2]\n"
   ++ concat [ "  " ++ show i ++ " -> " ++ show j
                    ++ "[label=" ++ escapeString label ++ "," ++ show dir ++ "];\n"
             | ((i,j),(dir,a)) <- Map.toList edges
             , let label = case (dir,a) of
                     (Self, _               ) -> "" -- no label on reflexive edges
                     (_,    Agents        xs) -> concat $ intersperse "," xs
                     (_,    ExcludeAgents []) -> "*"
                     (_,    ExcludeAgents xs) -> "~" ++ concat (intersperse "," xs)
             ]
  where showVals = escapeString . sep . map showVal
        sep []       = ""
        sep [a]      = a
        sep [a,b]    = a ++ "," ++ b
        sep (a:b:xs) = a ++ "," ++ b ++ ",\n" ++ sep xs
        showVal (v,True ) = v
        showVal (v,False) = "\xAC" ++ v
        -- join relations for different agents
        edges = Map.fromListWith plusEdge
                [ mkEdge i j a
                | (a,ijs) <- Map.toList (modRelations model)
                , (i,js)  <- Map.toList ijs, modIsWorld i model
                , j       <- js, modIsWorld j model
                ]

