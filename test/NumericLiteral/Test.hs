{- |
Module      : NumericLiteral.Test
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for 'NumericLiteral'.
-}
{-# LANGUAGE OverloadedStrings #-}
module NumericLiteral.Test where

import           NumericLiteral

import           Data.Attoparsec.Text
import           Test.Tasty                     (TestTree , testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "NumericLiteral"
  [ testGroup "numericLiteral"
    [ testCase "0"          $ Right "0"          @=? parseOnly (numericLiteral <* endOfInput) "0"
    , testCase "1234567890" $ Right "1234567890" @=? parseOnly (numericLiteral <* endOfInput) "1234567890"
    , testCase "1234.567"   $ Right "1234.567"   @=? parseOnly (numericLiteral <* endOfInput) "1234.567"
    , testCase "0x012abc"   $ Right "0x012abc"   @=? parseOnly (numericLiteral <* endOfInput) "0x012abc"
    ]
  , testGroup "nonDecimalIntegerLiteral"
    [ testCase "0x012abc"   $ Right "0x012abc"   @=? parseOnly (nonDecimalIntegerLiteral <* endOfInput) "0x012abc"
    ]
  ]
