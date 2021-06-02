{- |
Module      : Zipper.Test
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for 'Zipper'.
-}
{-# LANGUAGE OverloadedStrings #-}
module Zipper.Test where

import           Zipper

import           Test.Tasty                     (TestTree , testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Zipper"
  [ testGroup "rewind"
    [ testCase "Singleton"        $ Zip 'a' "" ""      @=? rewind (Zip 'a' "" "")
    , testCase "All the way left" $ Zip 'a' "" "bcdef" @=? rewind (Zip 'a' "" "bcdef")
    , testCase "Moves left"       $ Zip 'c' "ab" "def" @=? rewind (Zip 'd' "abc" "ef")
    ]
  , testGroup "advance"
    [ testCase "Singleton"         $ Zip 'a' "" ""      @=? advance (Zip 'a' "" "")
    , testCase "All the way right" $ Zip 'f' "abcde" "" @=? advance (Zip 'f' "abcde" "")
    , testCase "Moves left"        $ Zip 'd' "abc" "ef" @=? advance (Zip 'c' "ab" "def")
    ]
  , testGroup "seekBy"
    [ testCase "Singleton, not found"         $ Zip 'a' "" ""      @=? seekBy (const False) (Zip 'a' "" "")
    , testCase "Singleton, found"             $ Zip 'a' "" ""      @=? seekBy (const True ) (Zip 'a' "" "")
    , testCase "All the way right, not found" $ Zip 'f' "abcde" "" @=? seekBy (const False) (Zip 'f' "abcde" "")
    , testCase "All the way right, found"     $ Zip 'f' "abcde" "" @=? seekBy (const True ) (Zip 'f' "abcde" "")
    , testCase "All the way left, not found"  $ Zip 'f' "abcde" "" @=? seekBy (const False) (Zip 'a' "" "bcdef")
    , testCase "All the way left, found"      $ Zip 'c' "ab" "def" @=? seekBy (== 'c'     ) (Zip 'a' "" "bcdef")
    ]
  , testGroup "seek"
    [ testCase "Singleton, not found"         $ Zip 'a' "" ""      @=? seek '!' (Zip 'a' "" "")
    , testCase "Singleton, found"             $ Zip 'a' "" ""      @=? seek 'a' (Zip 'a' "" "")
    , testCase "All the way right, not found" $ Zip 'f' "abcde" "" @=? seek '!' (Zip 'f' "abcde" "")
    , testCase "All the way right, found"     $ Zip 'f' "abcde" "" @=? seek 'f' (Zip 'f' "abcde" "")
    , testCase "All the way left, not found"  $ Zip 'f' "abcde" "" @=? seek '!' (Zip 'a' "" "bcdef")
    , testCase "All the way left, found"      $ Zip 'c' "ab" "def" @=? seek 'c' (Zip 'a' "" "bcdef")
    ]
  , testGroup "fromList"
    [ testCase "Singleton"  $ Zip 'a' "" ""      @=? fromList "a"
    , testCase "Many items" $ Zip 'a' "" "bcdef" @=? fromList "abcdef"
    ]
  , testGroup "toList"
    [ testCase "Singleton"         $ "a"      @=? toList (Zip 'a' "" "")
    , testCase "All the way left"  $ "abcdef" @=? toList (Zip 'a' "" "bcdef")
    , testCase "All the way right" $ "abcdef" @=? toList (Zip 'f' "abcde" "")
    , testCase "In the middle"     $ "abcdef" @=? toList (Zip 'c' "ab" "def")
    ]
  ]
