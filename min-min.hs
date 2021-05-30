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


isWhitespace :: Token -> Bool
isWhitespace (Whitespace _) = True
isWhitespace _              = False

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x (y : ys) | x == y    = split x ys
                 | otherwise = let (y', ys') = span (/= x) (y : ys) in y' : split x ys'


main :: IO ()
main = do
  res <- tokenise <$> TIO.getContents
  case res of
    Right toks -> mapM_ print $ split EndOfLine $ filter (not . isWhitespace) toks
    Left  err  -> putStrLn $ "Parse failed: " ++ err
