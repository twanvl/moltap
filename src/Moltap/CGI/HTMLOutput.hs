{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.CGI.HTMLOutput
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Showing terms and errors as HTML.
--
--------------------------------------------------------------------------------

module Moltap.CGI.HTMLOutput
    ( ShowHTML(..)
    ) where

import Moltap.Base.Agents
import Moltap.Base.Syntax
import Moltap.Base.Proof
import Moltap.Util.Util

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

import qualified Data.Set as Set
import Data.List (intersperse)
import Prelude   hiding (span)

--------------------------------------------------------------------------------
-- Converting things to HTML
--------------------------------------------------------------------------------

-- | Things that can be converted to HTML
class ShowHTML a where
    showAsHTML :: a -> String
    showHTML   :: Int -> a -> ShowS
    showHTML _ a = showString (showAsHTML a)
    showAsHTML a = showHTML 0 a []

instance ShowHTML a => ShowHTML [a] where
    showHTML p = showListWith (showHTML p)

instance ShowHTML Char where
    showHTML _ '<' = showString "&lt;"
    showHTML _ '>' = showString "&gt;"
    showHTML _ '&' = showString "&amp;"
    showHTML _ x   = showChar x

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | A HTML attribute
data HTMLAttr = String := String

instance ShowHTML HTMLAttr where
     showHTML _ (k := v) = showString " " . showString k . showString "='" . showHTML 0 v . showString "'"

-- | Create a HTML tag with some attributes and a body
tag :: String -> [HTMLAttr] -> ShowS -> ShowS
tag t args body = showChar '<' . showString t . showHTML 0 args . showChar '>'
                . body
                . showString "</" . showString t . showChar '>'

span :: String -> ShowS -> ShowS
span cls c = tag "span" ["class":=cls] c

spanString :: String -> String -> ShowS
spanString cls = span cls . showString

-- | Wrap marked up parenthesis if the condition holds
showHParen :: Bool -> ShowS -> ShowS
showHParen True  s = spanString "par" "(" . s . spanString "par" ")"
showHParen False s = s

sp :: ShowS
sp = showChar ' '

--------------------------------------------------------------------------------
-- Formulas to HTML
--------------------------------------------------------------------------------

instance ShowHTML BinOp where
    showHTML _ And    = spanString "sym" "&and;" 
    showHTML _ Or     = spanString "sym" "&or;"  
    showHTML _ Imp    = spanString "sym" "&rarr;"
    showHTML _ Pmi    = spanString "sym" "&larr;"
    showHTML _ Equiv  = spanString "sym" "&harr;"
    showHTML _ Differ = spanString "sym" "&ne;"  

instance ShowHTML Formula where
    showHTML = formulaToHTML

formulaToHTML :: Int -> Formula -> ShowS
formulaToHTML pp tt = nesting pp tt $ showHTML' pp tt
  where
    showHTML' _ (Truth    t) = spanString "kw" (if t then "true" else "false")
    showHTML' p (Not      a) = showHParen (p > 70) $ spanString "sym" "&not;" . showHTML 70 a
    showHTML' p (Var      v) = showHTML p v
    showHTML' p (Bin  o a b) = showHParen (p > outer) $ showHTML left a . sp . showHTML 0 o . sp . showHTML right b
                               where (outer,left,right) = precedence o
    showHTML' p (Box  s a b) = showHParen (p > 70) $ span "ksym" (showString (if s then "K" else "M")                                 . showHTML 0 a) . sp . showHTML 70 b
    showHTML' p (Star s a b) = showHParen (p > 70) $ span "ksym" (showString (if s then "K" else "M") . tag "sup" [] (showString "*") . showHTML 0 a) . sp . showHTML 70 b
    showHTML' p (Note   _ a) = showHTML' p a
    
    -- Do we need to wrap a <span class='nest'>?
    needNest 60 (Bin And _ _) = False
    needNest 50 (Bin Or  _ _) = False
    needNest p  (Note   _  t) = needNest p t
    needNest _  _             = True
    
    -- Add a <span class="nest"> tag, possibly with information from notes
    nesting :: Int -> Formula -> ShowS -> ShowS
    nesting p t
       | needNest p t = tag "span" ("class":="nest":showNote t)
       | otherwise    = id

-- attributes for a node
showNote :: Formula -> [HTMLAttr]
showNote (Note (NoteTruth tr) _) = ["truth" := concat (intersperse " " [ (if true then "t" else "f") ++ var | (var,true) <- tr ]) ]
showNote _                       = []

instance ShowHTML Agents where
    showHTML _ (Agents        [x]) = tag "sub" [] $ showHTML 0 x
    showHTML _ (Agents        xs)  = tag "sub" [] $ showChar '{' . showListWithSep "," (showHTML 0) xs . showChar '}'
    showHTML _ (ExcludeAgents [])  = id
    showHTML _ (ExcludeAgents xs)  = tag "sub" ["class":="negate"] $ showChar '{' . showListWithSep "," (showHTML 0) xs . showChar '}'

instance ShowHTML Program where
    showHTML p (Formula      f) = showHTML p f
    showHTML p (Let'     v a b) = showHParen (p > 0)
                                $ tag "span" ("class":="nest":showNote a)
                                    ( spanString "kw" "let" . sp . showString v . sp
                                    . spanString "sym" "=" . sp . showHTML 10 a
                                    . spanString "sym" ";")
                                . showString "<br>\n" . showHTML 0 b
    showHTML p (System   l a b) = showHParen (p > 0)
                                $ spanString "kw" "system" . sp . shows a
                                . (if isAllAgents l then id
                                   else sp . spanString "kw" "for" . sp . showHTML 0 l)
                                . spanString "sym" ";"
                                . showString "<br>\n" . showHTML 0 b

--------------------------------------------------------------------------------
-- Proofs HTML
--------------------------------------------------------------------------------

showAxSign :: Sign -> ShowS
showAxSign False = tag "sup" [] $ showString "L"
showAxSign True  = tag "sup" [] $ showString "R"

instance ShowHTML ProofStep where
    showHTML _ (ProofTruth  s      ) = spanString "ax" (if s then "True" else "False")
    showHTML _ (ProofAssum    _    ) = spanString "ax" "Assum"
    showHTML _ (ProofCut      _    ) = spanString "ax" "Cut"
    showHTML _ (ProofNot    s _    ) = span "ax" $ spanString "sym" "&not;"  . showAxSign s
    showHTML _ (ProofBin    s o _ _) = span "ax" $ span "sym" (showHTML 0 o) . showAxSign s
    showHTML _ (ProofBox    _ a _  ) = span "ax" $ showString "Nec" . showHTML 0 a
    showHTML _ (ProofD      s a _  ) = span "ax" $ showString "D"   . showHTML 0 a . showAxSign s
    showHTML _ (ProofT      s a _  ) = span "ax" $ showString "T"   . showHTML 0 a . showAxSign s
    showHTML _ (Proof4      s a _  ) = span "ax" $ showString "4"   . showHTML 0 a . showAxSign s
    showHTML _ (Proof5      s a _  ) = span "ax" $ showString "5"   . showHTML 0 a . showAxSign s

instance ShowHTML Sequent where
    showHTML _ s = showSeqPart [ f | False :+ f <- Set.toList s ]
                 . spanString "sym" " &#8866; "
                 . showSeqPart [ f | True  :+ f <- Set.toList s ]
       where showSeqPart = showListWithSep ", " (formulaToHTML 0)

--instance ShowHTML WithSequent where

--------------------------------------------------------------------------------
-- Parser errors to HTML
--------------------------------------------------------------------------------

instance ShowHTML ParseError where
    showAsHTML e =
        "<h2>Parse error</h2>\n" ++
        "<ul>" ++
          "<li>" ++ (if line == 1
                        then "on column " ++ show col
                        else "on line " ++ show line ++ ", column " ++ show col)
                 ++ ": \"<tt>" ++ escapeInput (reverse . clip 12 . reverse $ before)
                 ++ "<span class='bad'>"      ++ escapeInput after1 ++ "</span>"
                 ++ "<span class='more-bad'>" ++ escapeInput after2 ++ "</span></tt>\""
          ++ (case escapeInput (showErrorMessages "or" "<li>unknown parse error"
                                         "<li>expecting" "<li>unexpected" "end of input" (errorMessages e)) of
                msg@('<':'l':'i':_) -> msg
                msg                 -> "<li>" ++ msg
             ) ++
        "</ul>"
      where 
         pos  = errorPos e
         col  = sourceColumn pos
         line = sourceLine   pos
         (before,after) = splitAtPos pos
         (after1,after2) = splitAt 1 $ clip 6 after
         clip n xs = case splitAt n xs of
                      (a,[]) -> a
                      (a,_)  -> a ++ "..."
         escapeInput []         = []
         escapeInput xs@('"':_) = case reads xs of
                                     [(str,rest)] -> "\"<tt>" ++ showAsHTML (str :: String) ++ "</tt>\"" ++ escapeInput rest
                                     _            -> '"' : escapeInput xs -- fail
         escapeInput (x:xs)     = x : escapeInput xs

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | Find the part before and after a SourcePos in a string (stored in sourceName)
--   The before part will be reversed
splitAtPos :: SourcePos -> (String,String)
splitAtPos pos = splitAtPos' (initialPos "") (sourceName pos)
  where splitAtPos' _  [] = ([]," ") -- end of input
        splitAtPos' p2 (x:xs)
          | sourceLine   pos /= sourceLine   p2 = splitAtPos' (updatePosChar p2 x) xs
          | sourceColumn pos /= sourceColumn p2 = let (before,after) = splitAtPos' (updatePosChar p2 x) xs in (x:before,after)
          | otherwise                           = ([],x:xs)
