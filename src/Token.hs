{- |
Module      : Token
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A minimal JavaScript minifier.
-}
{-# LANGUAGE OverloadedStrings #-}
module Token where

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.Text
import           Data.Char                      ( isSpace )
import           Data.Text                      ( Text )


data Token = Whitespace !Text
           | NotWhitespace !Text
           deriving (Eq, Ord, Show)


tokenise :: Text -> Either String [Token]
tokenise = parseOnly (many' parseToken <* endOfInput)

parseToken :: Parser Token
parseToken = parseWhitespace <|> parseNotWhitespace

parseWhitespace :: Parser Token
parseWhitespace = Whitespace <$> takeWhile1 isSpace

parseNotWhitespace :: Parser Token
parseNotWhitespace = NotWhitespace <$> takeWhile1 (not . isSpace)
