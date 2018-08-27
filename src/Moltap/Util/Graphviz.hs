{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Util.Graphviz
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions for invoking GraphViz
--
--------------------------------------------------------------------------------

module Moltap.Util.Graphviz
    ( GraphvizProgram(..)
    , NodePosition(..)
    , runGraphviz
    ) where

import Control.Exception
import System.IO.UTF8 (hPutStr)
import System.IO      (hGetContents, hClose)
import System.Process
import System.FilePath

import Moltap.Util.Util
import Moltap.Util.SimpleJSON

--------------------------------------------------------------------------------
-- Node positions
--------------------------------------------------------------------------------

-- | The position of a node in a rendered graph
data NodePosition
    = NodeCircle { npNode :: String, npX, npY, npRadius :: Int }

parseNodePosition :: String -> NodePosition
parseNodePosition = p . words . map (\x -> if x==',' then ' ' else x)
  where p ["circle",n,x,y,r] = NodeCircle n (read x :: Int) (read y :: Int) (abs (read r :: Int))
        p ("rect":  n:coords) -- older versions of graphviz output a rectangle for circle nodes
                             = NodeCircle n ((x1 + x2) `div` 2) ((y1 + y2) `div` 2) ((x2 - x1 + y2 - y1) `div` 4)
                                 where [x1,y1,x2,y2] = map read coords :: [Int]
        p _                  = error "unexpected shape from graphviz"

parseNodePositions :: String -> [NodePosition]
parseNodePositions = map parseNodePosition . filter (not . null) . drop 1 . lines

instance ToJSON NodePosition where
    toJSON (NodeCircle n x y r) = toJSON ["node" =: n, "x" =: x, "y" =: y, "r" =: r]

--------------------------------------------------------------------------------
-- Graphviz invokation
--------------------------------------------------------------------------------

-- | What Graphviz program to use for visualizing a graph
data GraphvizProgram = Dot | Neato | FDP

instance Show GraphvizProgram where
    show Dot   = "dot"
    show Neato = "neato"
    show FDP   = "fdp"

-- | Run 'dot' or 'neato' to convert a .dot file to .png
--
--   Returns the positions of the nodes
runGraphviz :: GraphvizProgram -> FilePath -> String -> IO [NodePosition]
runGraphviz prog filename graph = do
    (i,o,_e,proc) <- runInteractiveProcess (show prog)
                      [extToType filename, "-Timap", "-o", filename] Nothing Nothing
    hPutStr i ("digraph G{dpi=80;\n" ++ graph ++ "\n}")
    hClose  i
    out <- hGetContents o
    let positions = parseNodePositions out
    evaluate (length positions) -- force
    waitForProcess proc
    makeFileReadable filename
    return positions

-- | Type to use for graphiviz invokation, based on output filename
extToType :: FilePath -> String
extToType filename = case takeExtension filename of
    ""      -> "-Tpng"
    (_:ext) -> "-T" ++ ext
