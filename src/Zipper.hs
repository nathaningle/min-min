{- |
Module      : Zipper
Copyright   : (c) 2021 Nathan Ingle
Licence     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

A Zipper data structure.
-}
{-# LANGUAGE OverloadedStrings #-}
module Zipper where

import qualified Data.Foldable                 as F
import           Data.Sequence                  ( Seq((:<|), (:|>), Empty) )
import qualified Data.Sequence                 as Seq


-- | A non-empty zipper list.
data Zipper a = Zip a (Seq a) (Seq a) deriving (Eq, Ord, Show)

-- | Move a 'Zipper' to the left.
rewind :: Zipper a -> Zipper a
rewind z@(Zip _ Empty      _ ) = z
rewind (  Zip x (ls :|> l) rs) = Zip l ls (x :<| rs)

-- | Move a 'Zipper' to the right.
advance :: Zipper a -> Zipper a
advance z@(Zip _ _  Empty     ) = z
advance (  Zip x ls (r :<| rs)) = Zip r (ls :|> x) rs

-- | Advance a 'Zipper' until the focused item satisfies the given predicate.
seekBy :: (a -> Bool) -> Zipper a -> Zipper a
seekBy _ z@(Zip _ _ Empty) = z
seekBy f z@(Zip x _ _) | f x       = z
                       | otherwise = seekBy f $ advance z

-- | Advance a 'Zipper' to focus on the next occurrence of a given item.
seek :: Eq a => a -> Zipper a -> Zipper a
seek target = seekBy (== target)

-- | Convert a list to a 'Zipper'.  Passing an empty list results in a runtime error.
fromList :: [a] -> Zipper a
fromList []       = error "can't make a zipper from an empty list"
fromList (t : ts) = Zip t Empty (Seq.fromList ts)

-- | Convert a 'Zipper' to a list.
toList :: Zipper a -> [a]
toList (Zip x ls rs) = F.toList ls ++ x : F.toList rs
