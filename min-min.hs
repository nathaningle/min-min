{- |
File        : min-min.hs
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A minimal JavaScript minifier.
-}
import           Token                          ( tokenise )
import           Minify                         ( minify
                                                , renameVars
                                                , render
                                                )

import qualified Data.Text.IO                  as TIO


main :: IO ()
main = do
  res <- tokenise <$> TIO.getContents
  case res of
    Right toks -> TIO.putStrLn $ render $ renameVars $ minify toks
    Left  err  -> putStrLn $ "Parse failed: " ++ err
