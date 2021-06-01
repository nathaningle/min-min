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

import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


-- | Convert a list of 'Token's back to JavaScript (minimally, rather than prettily).
render :: [Token] -> Text
render = foldl' (\acc tok -> acc <> renderToken tok) ""

-- | Convert a single 'Token' back to JavaScript.
renderToken :: Token -> Text
renderToken Whitespace         = " "
renderToken EndOfLine          = "\n"
renderToken (Comment        s) = s
renderToken (IdentifierName s) = s
renderToken (Punctuator     s) = s
renderToken (NumericLiteral s) = s
renderToken (RegExLiteral   s) = s
renderToken (StringLiteral  s) = s


-- | Ditch as many 'Token's as we can whilst retaining the same meaning.
minify :: [Token] -> [Token]
minify = dropWhile (== Whitespace) . minify' . filter isWanted
 where
  isWanted EndOfLine   = False
  isWanted (Comment _) = False
  isWanted _           = True

-- | Helper function for 'minify'.
minify' :: [Token] -> [Token]
minify' []           = []
minify' [Whitespace] = []
minify' (Whitespace : Whitespace : toks) = minify' (Whitespace : toks)
minify' (Whitespace : tok@(Punctuator _) : toks) = minify' (tok : toks)
minify' (tok@(Punctuator _) : Whitespace : toks) = minify' (tok : toks)
minify' (tok : toks) = tok : minify' toks


-- | Rename variables that are declared with @myvar = @.
renameVars :: [Token] -> [Token]
renameVars = renameVars' M.empty Nothing

-- | Helper function for 'renameVars'.
renameVars' :: Map Text Text -> Maybe Text -> [Token] -> [Token]
renameVars' _ Nothing        [] = []
renameVars' _ (Just varName) [] = [IdentifierName varName]
renameVars' dict (Just varName) (Punctuator "=" : toks) =
  IdentifierName varName' : Punctuator "=" : renameVars' dict' Nothing toks
  where (varName', dict') = insertNext varName dict
renameVars' dict Nothing (IdentifierName varName : toks)
  | isReservedWord varName = IdentifierName varName : renameVars' dict Nothing toks
  | otherwise = case M.lookup varName dict of
    Nothing       -> renameVars' dict (Just varName) toks
    Just varName' -> IdentifierName varName' : renameVars' dict Nothing toks
renameVars' dict Nothing    (tok : toks) = tok : renameVars' dict Nothing toks
renameVars' dict (Just old) toks         = IdentifierName old : renameVars' dict Nothing toks

-- | Generate a new short variable name and insert the mapping from long name
-- to short name into the dictionary.
insertNext :: Text -> Map Text Text -> (Text, Map Text Text)
insertNext varName dict | null dict = ("a", M.insert varName "a" dict)
                        | otherwise = (varName', M.insert varName varName' dict)
  where varName' = nextVarName (maximum (M.elems dict))

-- | Identify the name that succeeds the given one.
nextVarName :: Text -> Text
nextVarName varName | isReservedWord varName' = nextVarName varName'
                    | otherwise               = varName'
  where varName' = intToVarName $ (+ 1) $ varNameToInt varName

-- | Convert an 'Int' to its corresponding short variable name: 0 = @a@,
-- 25 = @z@, 26 = @aa@, 27 = @ab@ etc.
intToVarName :: Int -> Text
intToVarName = T.pack . reverse . intToVarName'

-- | Helper function for 'intToVarName'.
intToVarName' :: Int -> String
intToVarName' x | x < 0     = error "invalid varName enumeration"
                | x < 26    = [chr (x + 0x61)]
                | otherwise = chr (r + 0x61) : intToVarName' (q - 1)
  where (q, r) = x `quotRem` 26

-- | Convert a short variable name to an 'Int'.  The inverse of 'intToVarName'.
varNameToInt :: Text -> Int
varNameToInt = foldl' go (negate 1) . T.unpack
 where
  go :: Int -> Char -> Int
  go acc c = (acc + 1) * 26 + ord c - 0x61


-- | Returns 'True' iff the given word is reserved (see 'reservedWords').
isReservedWord :: Text -> Bool
isReservedWord w = w `S.member` reservedWords

-- | Reserved words per the [ECMAscript standard](https://262.ecma-international.org/11.0/#prod-ReservedWord).
-- These words may not be used as variable names.
reservedWords :: Set Text
reservedWords = S.fromList
  [ "await"
  , "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "enum"
  , "export"
  , "extends"
  , "false"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "null"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "true"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  , "yield"
  ]
