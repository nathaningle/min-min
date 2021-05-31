{- |
Module      : Minify
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Write out minified JavaScript.
-}
{-# LANGUAGE OverloadedStrings #-}
module Minify where

import           Token                          ( Token(..) )

import           Data.Foldable                  ( foldl' )
import           Data.Text                      ( Text )


render :: [Token] -> Text
render = foldl' (\acc tok -> acc <> renderToken tok) ""

renderToken :: Token -> Text
renderToken Whitespace         = " "
renderToken EndOfLine          = "\n"
renderToken (Comment        s) = s
renderToken (IdentifierName s) = s
renderToken (Punctuator     s) = s
renderToken (NumericLiteral s) = s
renderToken (RegExLiteral   s) = s
renderToken (StringLiteral  s) = s


minify :: [Token] -> [Token]
minify = dropWhile (== Whitespace) . minify' . filter isWanted
  where isWanted t = not $ t == EndOfLine || isComment t

minify' :: [Token] -> [Token]
minify' []           = []
minify' [Whitespace] = []
minify' (Whitespace : Whitespace : toks) = minify' (Whitespace : toks)
minify' (Whitespace : tok@(Punctuator _) : toks) = minify' (tok : toks)
minify' (tok@(Punctuator _) : Whitespace : toks) = minify' (tok : toks)
minify' (tok : toks) = tok : minify' toks

isComment :: Token -> Bool
isComment (Comment _) = True
isComment _           = False
