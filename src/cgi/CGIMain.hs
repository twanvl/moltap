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

import Control.Exception
import Data.Monoid

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64

import Data.Aeson as JSON hiding (json)
import Network.Wai
import Network.Wai.Handler.CGI
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Jsonp
import Network.Wai.Parse
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

timelimit :: Int
timelimit = 10000000 -- 10 seconds

--------------------------------------------------------------------------------
-- CGI utilities
--------------------------------------------------------------------------------

getParameters :: Request -> IO [(B.ByteString,B.ByteString)]
getParameters req = do
  let parseOpts = setMaxRequestNumFiles 0 $ defaultParseRequestBodyOptions
  (params,_) <- parseRequestBodyEx parseOpts lbsBackEnd req
  let query = queryString req
  return $ params ++ [(x,y) | (x,Just y) <- query]

decodeUtf8 :: B.ByteString -> String
decodeUtf8 = Text.unpack . Text.decodeUtf8

--------------------------------------------------------------------------------
-- CGI main program
--------------------------------------------------------------------------------

main :: IO ()
main = run $ middleware app

middleware :: Middleware
middleware = gzip def . jsonp

app :: Application
app req respond = do
    args <- getParameters req
    case lookup "term" args of
      Nothing -> respondError badRequest400 "Missing parameter 'term'"
      Just termString -> case tryParse (decodeUtf8 termString) of
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
              (positions,pngData) <- runGraphvizToMemory Neato "png" $ modelToDot model
              let modelImage = encodeAsDataURI "image/png" pngData
              respondJSON ok200 $ JSON.object
                 ["result"     .= False
                 ,"text"       .= showAsHTML (annotate model term)
                 ,"modelDot"   .= modelToDot model
                 ,"modelImage" .= modelImage
                 ,"modelPos"   .= positions
                 ]
  `catch` \e -> do
    respondError internalServerError500 (show (e :: SomeException))
  where
  headers = [(hContentType,"application/json")]
  respondJSON :: Status -> JSON.Value -> IO ResponseReceived
  respondJSON _status json =
    respond $ responseLBS status headers $ encode json
    where status = ok200 -- note: always respond with 200 OK if possible, to get the error message in the javascript code
  respondError :: Status -> String -> IO ResponseReceived
  respondError status message =
    respondJSON status $ JSON.object ["result" .= ("error" :: String), "text" .= message]

encodeAsDataURI :: Text.Text -> B.ByteString -> Text.Text
encodeAsDataURI mimeType content = "data:" <> mimeType <> ";base64," <> Text.decodeUtf8 (Base64.encode content)

