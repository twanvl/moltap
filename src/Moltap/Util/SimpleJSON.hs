{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Util.SimpleJSON
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A very simple output library for JSON (JavaScript Object Notation).
--
--------------------------------------------------------------------------------

module Moltap.Util.SimpleJSON
    ( JSON(..), JSONKeyVal
    , (=:)
    , ToJSON(..)
    ) where

import Data.Char
import Moltap.Util.Util

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | JSON data.
--
--   The data can be converted to actual JSON notation using 'show'.
data JSON
    = JSONString String
    | JSONInt    Int
    | JSONBool   Bool
    | JSONList   [JSON]
    | JSONObject [JSONKeyVal]

-- | A JSON key\/value pair, a list of these forms an object.
data JSONKeyVal = JSONKeyVal String JSON

infix 0 =:

-- | Create a key\/value pair for in a JSONObject.
--
--   Objects can be constructed using
--
--   > ["x" =: value1, "y" =: value2]
--
--   Which converts to
--
--   > {x: value1, y: value2}
(=:) :: ToJSON a => String -> a -> JSONKeyVal
k =: v  =  JSONKeyVal k (toJSON v)

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

instance Show JSON where
    showsPrec _ (JSONString   s) = showString (escapeString s)
    showsPrec _ (JSONInt      i) = shows i
    showsPrec _ (JSONBool False) = showString "false"
    showsPrec _ (JSONBool  True) = showString "true"
    showsPrec _ (JSONList    xs) = showChar '[' . showJSONList xs . showChar ']'
    showsPrec _ (JSONObject  xs) = showChar '{' . showJSONList xs . showChar '}'

instance Show JSONKeyVal where
    showsPrec _ (JSONKeyVal k v) = showKey k . showString ": " . shows v
      where showKey xxs@(x:xs) | isAlpha x && all isAlphaNum xs = showString xxs
            showKey xxs                                         = shows xxs

showJSONList :: (Show a) => [a] -> String -> String
showJSONList xs
  | len < 50  = showListWithSep ", "  shows xs
  | otherwise = showListWithSep "\n," shows xs
 where len = sum $ map (succ . length . show) xs

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

-- | Things that can be converted to JSON
class ToJSON a where
    -- | Convert a value to 'JSON'
    toJSON     ::  a  -> JSON
    -- | Convert a list to 'JSON'
    toJSONList :: [a] -> JSON
    toJSONList = JSONList . map toJSON

instance ToJSON JSON where
    toJSON     = id
    toJSONList = JSONList

instance ToJSON JSONKeyVal where
    toJSON     = JSONObject . return
    toJSONList = JSONObject

instance ToJSON Char where
    toJSON     = JSONString . return
    toJSONList = JSONString

instance ToJSON Int where
    toJSON = JSONInt

instance ToJSON Bool where
    toJSON = JSONBool

instance ToJSON a => ToJSON [a] where
    toJSON = toJSONList
