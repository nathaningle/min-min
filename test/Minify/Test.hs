{- |
Module      : Minify.Test
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for 'Minify'.
-}
{-# LANGUAGE OverloadedStrings #-}
module Minify.Test where

import           Minify
import           Token                          ( Token(..) )

import           Test.Tasty                     (TestTree , testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Minify"
  [ testGroup "render"
    [ testCase "Empty list" $ "" @=? render []
    , testCase "Ordering" $ let toks = [ IdentifierName "let"
                                       , Whitespace, IdentifierName "x"
                                       , Whitespace, Punctuator "="
                                       , Whitespace, NumericLiteral "100"
                                       , Punctuator ";", EndOfLine]
                            in "let x = 100;\n" @=? render toks
    ]
  ]
