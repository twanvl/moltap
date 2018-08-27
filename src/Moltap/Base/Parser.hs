{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Moltap.Base.Parser
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  GPL 2 or later
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for modal formulas
--
--------------------------------------------------------------------------------

module Moltap.Base.Parser
    ( unsafeParse, tryParse, tryParseNamed
    , Parsable(..)
    ) where

import Moltap.Base.Syntax
import Moltap.Base.Agents

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Monad.Error
import Control.Applicative hiding (many,(<|>),optional)
import Data.Char

--------------------------------------------------------------------------------
-- Parsing : wrappers
--------------------------------------------------------------------------------

-- | Parse something, throw an error if it fails
unsafeParse :: Parsable a => String -> a
unsafeParse xs = case tryParseNamed "input" xs of
          Left  e -> error $ show e
          Right r -> r

-- | Parse something, return either an error, or the result
tryParse :: Parsable a => String -> Either ParseError a
tryParse xs = tryParseNamed xs xs

-- | Parse something with a name, return either an error, or the result
tryParseNamed :: Parsable a => String -> String -> Either ParseError a
tryParseNamed name xs = parse (parser <* eof) name xs

-- | Things that can be parsed
class Parsable a where
    parser :: CharParser () a

instance Applicative (GenParser a b) where
    (<*>) = ap
    pure = return

----------------------------------------------------------------
-- Parsing
----------------------------------------------------------------

-- Our tokenizer
tok :: TokenParser ()
tok = makeTokenParser haskellDef
    { commentLine     = "#"
    , reservedNames   = ["true","false","not","or","and","implies","let","system"]
    , reservedOpNames = ["~","|","||","&","&&",">","->","=>","<->","=","==","<=>","!=","/=","<-/->"
                        ,"\xAC","\x2227","\x2228","\x2192","\x2190","\x2194","\x21AE","\x22A4","\x22A5"]
    , caseSensitive   = False
    }

instance Parsable Formula where parser = parseFormula
instance Parsable Program where parser = whiteSpace tok *> parseProgram

sp :: CharParser () Char
sp = space <?> ""
gchar :: Char -> CharParser () ()
gchar c = char c >> return ()

parseProgram :: CharParser () Program
parseProgram
     =  Let' <$ reserved tok "let" <*> identifier tok <* reservedOp tok "=" <*> parseFormula <* semi tok <*> parseProgram
    <|> System allAgents <$ reserved tok "system" <*> parseAxiomSet                          <* semi tok <*> parseProgram
    <|> Formula <$> parseFormula
  <?> "top level formula"

parseAxiomSet :: CharParser () AxiomSet
parseAxiomSet = (p . map toUpper =<< identifier tok) <?> "axiom set"
  where p "S5" = return systemS5
        p "S4" = return systemS4
        p ax   = foldr ($) systemK <$> mapM pc ax
         where pc 'K' = return $ id
               pc 'T' = return $ \x -> x{ axT = True }
               pc 'D' = return $ \x -> x{ axD = True }
               pc '4' = return $ \x -> x{ ax4 = True }
               pc '5' = return $ \x -> x{ ax5 = True }
               pc _   = fail $ ax ++ " is not an axiom system"

parseFormula :: CharParser () Formula
parseFormula = buildExpressionParser
    [ [flip Infix AssocLeft  $ Bin And    <$ (reservedOps ["&","&&","\x2227"]      <|> reserved tok "and"     <?> "operator")]
    , [flip Infix AssocLeft  $ Bin Or     <$ (reservedOps ["|","||","\x2228"]      <|> reserved tok "or"      <?> "operator")]
    , [flip Infix AssocRight $ Bin Imp    <$ (reservedOps [">","->","=>","\x2192"] <|> reserved tok "implies" <?> "operator")
      ,flip Infix AssocLeft  $ Bin Pmi    <$ (reservedOps ["<","<-","<=","\x2190"]                            <?> "operator")
      ,flip Infix AssocNone  $ Bin Equiv  <$ (reservedOps ["<->","<=>","=","==","\x2194"]                     <?> "operator")
      ,flip Infix AssocNone  $ Bin Differ <$ (reservedOps ["!=","/=","<-/->","\x21AE"]                        <?> "operator")]
    ]
    (whiteSpace tok *> parseBaseTerm)
  <?> "formula"
 where reservedOps = foldr1 (<|>) . map (reservedOp tok)

parseAgent :: CharParser () Agent
parseAgent = lexeme tok ((:) <$> (lower <|> digit) <*> many alphaNum)
          <?> "agent name"

parseAgents :: CharParser () [Agent]
parseAgents = optional (char '_') >> whiteSpace tok >>
                ( return <$> parseAgent
              <|> braces tok (commaSep tok parseAgent)
                )
          <?> "agent names"

parseBaseTerm :: CharParser () Formula
parseBaseTerm = do sign <- choice [ char 'K' >> return True
                                  , char 'M' >> return False
                                  , char '\x25A1' >> return True
                                  , char '\x25C7' >> return False
                                  , string "[]" >> return True
                                  , string "<>" >> return False
                                  ]
                   box <- option Box (char '*' >> return Star)
                   try  (box sign . listAgents <$> parseAgents <* whiteSpace tok <*> parseBaseTerm)
                    <|> (box sign allAgents <$ many sp <*> parseBaseTerm)
            <|> Not         <$ (gchar '~' <|> gchar '\xAC' <|> reserved tok "not") <* whiteSpace tok <*> parseBaseTerm
            <|> Truth True  <$ (reserved tok "true"  <|> reservedOp tok "\x22A5")
            <|> Truth False <$ (reserved tok "false" <|> reservedOp tok "\x22A4")
            <|> char '0'    *> fail "Use \"false\" for the logical falsehood"
            <|> char '1'    *> fail "Use \"true\" for the logical truth"
            <|> upper       *> fail "Identifiers must start with lower case characters"
            <|> Var         <$> identifier tok
            <|> parens tok parseFormula
          <?> "formula"
