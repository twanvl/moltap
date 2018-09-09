{-# LANGUAGE OverloadedStrings #-}
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
    , runGraphviz, runGraphviz_
    , runGraphvizToMemory
    ) where

import Control.Monad
import Control.Exception
import System.IO
import System.Process
import System.FilePath
import qualified Data.ByteString.Char8 as B
import Data.Aeson (ToJSON(..),object,(.=))
import Data.Char

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
    toJSON (NodeCircle n x y r) = object ["node" .= n, "x" .= x, "y" .= y, "r" .= r]

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
runGraphviz prog filename graphDot = do
    (i,o,_e,proc) <- runInteractiveProcess (show prog)
                      [extToType filename, "-Timap", "-o", filename] Nothing Nothing
    hSetEncoding i utf8
    hPutStr i graphDot
    hClose  i
    out <- hGetContents o
    let positions = parseNodePositions out
    _ <- evaluate (length positions) -- force
    _ <- waitForProcess proc
    return positions

-- | Run 'dot' or 'neato' to convert a .dot file to .png
--
--   Returns the positions of the nodes, and the image file contents
runGraphvizToMemory :: GraphvizProgram -> String -> String -> IO ([NodePosition],B.ByteString)
runGraphvizToMemory prog format dot =
  let theProc = ((proc (show prog) ["-Timap","-T"++format]) { std_in = CreatePipe, std_out = CreatePipe })
  in withCreateProcess theProc $ \(Just i) (Just o) _ proc -> do
  hSetEncoding i utf8
  hPutStr i dot
  hClose i
  hSetBinaryMode o True
  out <- B.hGetContents o
  _ <- waitForProcess proc
  let (posData,imageData) = splitGraphvizOutput out
  return (parseNodePositions $ B.unpack posData, imageData)

runGraphviz_ :: GraphvizProgram -> FilePath -> String -> IO ()
runGraphviz_ prog filename graph = void (runGraphviz prog filename graph)

-- | Type to use for graphiviz invokation, based on output filename
extToType :: FilePath -> String
extToType filename = case takeExtension filename of
    ""      -> "-Tpng"
    (_:ext) -> "-T" ++ ext

-- | Split the output: positions first, then the remainder (png or svg)
splitGraphvizOutput :: B.ByteString -> (B.ByteString,B.ByteString)
splitGraphvizOutput = 
  B.span (\x -> isSpace x || isPrint x && x `notElem` ['<','>'])

