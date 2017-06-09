{-# LANGUAGE FlexibleContexts #-}
module Data.Bool2sparql.Parser
  (-- * Parsing function
    parseExpr
  )
where

import Control.Monad
import Data.BoolExpr
import Text.ParserCombinators.Parsec


-- | Query Parser = ParserBool (see BoolExpr) AND parseExpr
parseExpr :: CharParser st String
parseExpr = quotedString <|> identifier

-- | Parser for expr in quotes
identifier :: CharParser st String
identifier = do
    try whiteSpace
    str <- P.identifier lexer
    try whiteSpace
    pure ("\'" ++ str ++ "\'")

-- | Parser for expr in quotes
quotedString :: CharParser st String
quotedString = do
    try whiteSpace
    char '\''
    str <- many (noneOf "'")
    -- str <- many (noneOf "\'" <|> (char '\\' *> char '\''))
    char '\''
    try whiteSpace
    pure ("\'" ++ str ++ "\'")

