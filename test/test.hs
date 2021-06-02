{- |
File        : test.hs
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for @min-min@.
-}
import qualified Minify.Test
import qualified NumericLiteral.Test
import qualified Zipper.Test

import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Minify.Test.tests
  , NumericLiteral.Test.tests
  , Zipper.Test.tests
  ]
