--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.CLI.CLIMain
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple command line main program
--
--------------------------------------------------------------------------------

module Main (main) where

import Moltap.Base.Parser
import Moltap.Base.ModelGraphviz
import Moltap.Prover.Prover
import Moltap.Util.Graphviz

import Data.Char
import System.Console.GetOpt
import System.Environment
import System.IO

--------------------------------------------------------------------------------
-- Configuration options
--------------------------------------------------------------------------------

data ProgramOpt = Opt
    { invokation :: ProgramInvokation
    , modelName  :: FilePath
    }

data ProgramInvokation
    = Stdin
    | File         FilePath
    | Formula      String
    | Interactive
    | Help

defaultProgramOpt :: ProgramOpt
defaultProgramOpt = Opt Stdin "model.png"

setInvokation :: ProgramInvokation -> ProgramOpt -> ProgramOpt
setInvokation i o = o { invokation = i }

setModelName :: FilePath -> ProgramOpt -> ProgramOpt
setModelName m o = o { modelName = m }

options :: [OptDescr (ProgramOpt -> ProgramOpt)]
options =
    [ Option ['?'] ["help"]        (NoArg  $ setInvokation Help)                 "Show help page"
    , Option ['i'] ["interactive"] (NoArg  $ setInvokation Interactive)          "Interactive operation"
    , Option ['f'] ["formula"]     (ReqArg (setInvokation . Formula) "FORMULA")  "Give a formula directly on the command line"
    , Option ['o'] ["model-name"]  (ReqArg (setModelName)            "FILE")     "Filename for generated model images, default \"model.png\".\nThe extension determines the generated image type."
    ]

--------------------------------------------------------------------------------
-- Main program
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetEncoding stdin  utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    case getOpt (ReturnInOrder $ setInvokation . File) options args of
        (o,[],[]) -> let Opt invokeType modelFile = foldr ($) defaultProgramOpt o
                     in case invokeType of
             Help        -> wrong ""
             Stdin       -> run modelFile "stdin" =<< getContents
             File      f -> run modelFile f       =<< readFile f
             Formula   f -> run modelFile "interactive" f
             Interactive -> runInteractive modelFile
        (_,_,errs) -> wrong  (concat errs)
 where
   header = "Usage: moltap [OPTIONS] [FILE]"
   wrong message = putStrLn (message ++ usageInfo header options)

run :: FilePath -> String -> String -> IO ()
run modelFile inputName input =
    case tryParseNamed inputName input of
       Left e     -> putStrLn $ show e
       Right prog -> case prove prog of
           Left   _prf -> do
               putStrLn "true"
           Right model -> do
               putStrLn "false"
               -- Render the model with graphviz
               runGraphviz_ Neato modelFile $ modelToDot model
               return ()

runInteractive :: FilePath -> IO ()
runInteractive modelFile = loop
 where
  loop = do
    putStr "> "
    line <- getLine
    case line of
        ""   -> putStrLn "Type :q to quit" >> loop
        ":?" -> putStrLn "Type :q to quit" >> loop
        ":q" -> putStrLn "Goodbye"
        f    -> loopPartial f
  loopPartial f
    | incomplete f = do putStr ".. "
                        line <- getLine
                        loopPartial (f ++ "\n" ++ line)
    | otherwise    = run modelFile "formula" f >> loop

-- | Is the formula incomplete?
--    * Are there more open than close parantheses?
--    * Is it of the form "let x = y;"?
incomplete :: String -> Bool
incomplete f = tooManyOpen f || isStatement (reverse f)
  where 
    tooManyOpen = (> 0) . sum . map countParen
    countParen '(' = 1 :: Int
    countParen ')' = -1
    countParen _   = 0
    isStatement xs = case dropWhile isSpace xs of
                    []    -> True
                    (x:_) -> x == ';'
