--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.CGI.CGIMain
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Main program for invokation with CGI.
-- The result will be in JSON format
--
--------------------------------------------------------------------------------

module Main (main) where

import Moltap.Base.Parser
import Moltap.Base.Model
import Moltap.Base.ModelGraphviz
import Moltap.Base.HTMLOutput
import Moltap.Prover.Prover
import Moltap.Util.SimpleJSON
import Moltap.Util.Graphviz
import Moltap.Util.Util

import Control.Exception

import Network.CGI
import Codec.Binary.UTF8.String

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

modelImageDir :: FilePath
modelImageDir = "model/"

timelimit :: Int
timelimit = 10000000 -- 10 seconds

--------------------------------------------------------------------------------
-- JSON utilities
--------------------------------------------------------------------------------

-- | Output Json, optionally with a JsonP callback
outputJSON :: ToJSON a => a -> CGI CGIResult
outputJSON value = do
    callback <- getInput "callback"
    output $ wrapJsonP callback $ show $ toJSON value

-- | Optionally wrap a JsonP callback around a result
wrapJsonP :: Maybe String -> String -> String
wrapJsonP Nothing  xs = xs
wrapJsonP (Just f) xs = f ++ "(" ++ xs ++ ")"

-- | Get a parameter, fail if it is not set
getInputOrFail :: String -> CGI String
getInputOrFail name = do
    x <- getInput name
    case x of
       Nothing -> errorIO $ "Missing paramter '" ++ name ++ "'"
       Just  v -> return v

--------------------------------------------------------------------------------
-- CGI main program
--------------------------------------------------------------------------------

main :: IO ()
main = runCGI (handleErrors cgiMain)

cgiMain :: CGI CGIResult
cgiMain = do
    setHeader "Content-Type"   "text/plain"
    setHeader "Access-Control" "allow <*>"
    -- Parse and run
    json <- jsonMain `catchCGI` \e -> return ["result" =: "error", "text" =: show e]
    -- output the result
    outputJSON json

jsonMain :: CGI [JSONKeyVal]
jsonMain = do
    termString <- getInputOrFail "term"
    liftIO $ case tryParse (decodeString termString) of -- input is utf8 encoded
       Left e     -> errorIO $ showAsHTML e
       Right term -> do
         result <- timeoutWith timelimit (errorIO "Time limit exceeded")
                    $ evaluate (prove term)
         case result of
           Left   _prf -> do
               -- The term is true, just return it
               return ["result" =: True
                      ,"text"   =: showAsHTML term
                      ]
           Right model -> do
               -- Render the model with graphviz
               let name = modelImageDir ++ toFileName (show term) ++ ".png"
               positions <- runGraphviz Neato name $ modelToDot model
               return ["result"    =: False
                      ,"text"      =: showAsHTML (annotate model term)
                      ,"modelFile" =: name
                      ,"modelPos"  =: positions
                      ]
