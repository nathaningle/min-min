{- |
File        : min-min.hs
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A minimal JavaScript minifier.
-}
import           Token

import qualified Data.Text.IO                  as TIO


main :: IO ()
main = print =<< tokenise <$> TIO.getContents
