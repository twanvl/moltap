{-# LANGUAGE OverloadedStrings #-}
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
import Moltap.Util.Graphviz
import Moltap.Util.Util

import Control.Monad
import Control.Exception

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Base64 as Base64

import Data.Aeson as JSON hiding (json)
import Network.Wai
import Network.Wai.Handler.CGI
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Jsonp
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

modelImageDir :: FilePath
modelImageDir = "model/"

timelimit :: Int
timelimit = 10000000 -- 10 seconds

--------------------------------------------------------------------------------
-- CGI main program
--------------------------------------------------------------------------------

main :: IO ()
main = run $ middleware app

middleware :: Middleware
middleware = gzip def . jsonp

app :: Application
app req respond = do
    let args = queryToQueryText (queryString req)
    case join $ lookup "term" args of
      Nothing -> respondError badRequest400 "Missing parameter 'term'"
      Just termString -> case tryParse (Text.unpack termString) of
        Left e     -> respondError badRequest400 $ showAsHTML e
        Right term -> timeoutWith timelimit (respondError serviceUnavailable503 "Time limit exceeded") $ do
          result <- evaluate (prove term)
          case result of
            Left   _prf -> do
              -- The term is true, just return it
              respondJSON ok200 $ JSON.object
                ["result" .= True
                ,"text"   .= showAsHTML term
                ]
            Right model -> do
               -- Render the model with graphviz
              let name = modelImageDir ++ toFileName (show term) ++ ".png"
              --(positions,pngData) <- runGraphviz Neato name $ modelToDot model
              let positions = ["TODO" :: String]
              respondJSON ok200 $ JSON.object
                 ["result"    .= False
                 ,"text"      .= showAsHTML (annotate model term)
                 ,"modelFile" .= name
                 ,"modelPos"  .= positions
                 ]
  `catch` \e -> do
    respondError internalServerError500 (show (e :: SomeException))
  where
  headers = 
    [(hContentType,"application/json")]
  respondJSON :: Status -> JSON.Value -> IO ResponseReceived
  respondJSON status json =
    respond $ responseLBS status headers $ encode json
  respondError :: Status -> String -> IO ResponseReceived
  respondError status message =
    respondJSON status $ JSON.object ["result" .= ("error" :: String), "text" .= message]

