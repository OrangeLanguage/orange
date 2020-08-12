--  Copyright 2017 Paul Young
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

module Prettier.Printer
  ( DOC
  , Doc
  -- , be
  , below
  , beside
  , besideOrBelow
  -- , best
  -- , better
  , bracket'
  , bracket
  -- , copy
  , fill
  , fillwords
  -- , fits
  -- , flatten
  , folddoc
  , group
  , layout
  , line
  , nest
  , nil
  , pretty
  , spread
  , stack
  , text
  , words
  -- , (:<>)
  -- , (:<|>)
  , (<+>)
  , (</>)
  , (<+/>)
  , colorize
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List(Cons), (:))
import Data.List as List
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))

foreign import colorize :: String -> String -> String

data DOC
  = NIL
  | APPEND DOC DOC
  | NEST Int DOC
  | TEXT String String
  | LINE
  | UNION DOC DOC

infixr 5 UNION as :<|>
infixr 6 APPEND as :<>

instance semigroupDOC :: Semigroup DOC where
  append = APPEND

instance monoidDOC :: Monoid DOC where
  mempty = NIL

data Doc
  = Nil
  | Text String String Doc
  | Line Int Doc

nil :: DOC
nil = NIL

nest :: Int -> DOC -> DOC
nest = NEST

text :: String -> String -> DOC
text = TEXT

line :: DOC
line = LINE

group :: DOC -> DOC
group x = flatten x :<|> x

flatten :: DOC -> DOC
flatten NIL = NIL
flatten (APPEND x y) = flatten x :<> flatten y
flatten (NEST i x) = NEST i $ flatten x
flatten t@(TEXT _ _) = t
flatten LINE = TEXT "" " " 
flatten (x :<|> y) = flatten x

layout :: Doc -> String
layout Nil = ""
layout (Text color s x) = colorize color s <> layout x
layout (Line i x) = "\n" <> copy i " " <> layout x

copy :: Int -> String -> String
copy i x = intercalate "" $ Array.replicate i x

best :: Int -> Int -> DOC -> Doc
best w k x = be w k $ List.singleton (Tuple 0 x)

be :: Int -> Int -> List (Tuple Int DOC) -> Doc
be w k List.Nil = Nil
be w k (Cons (Tuple i NIL) z) = be w k z
be w k (Cons (Tuple i (APPEND x y)) z) = be w k $ (Tuple i x) : (Tuple i y) : z
be w k (Cons (Tuple i (NEST j x)) z) = be w k  $ (Tuple (i + j) x) : z
be w k (Cons (Tuple i (TEXT color s)) z) = Text color s $ be w (k + String.length s) z
be w k (Cons (Tuple i LINE) z) = Line i $ be w i z
be w k (Cons (Tuple i (UNION x y)) z) =
  let x' = be w k $ (Tuple i x) : z
  in if fits (w - k) x' then x' else be w k $ (Tuple i y) : z

fits :: Int -> Doc -> Boolean
fits w x | w < 0 = false
fits w Nil = true
fits w (Text color s x) = fits (w - String.length s) x
fits w (Line i x) = true

pretty :: Int -> DOC -> String
pretty w x = layout $ best w 0 x

-- Utility functions

beside :: DOC -> DOC -> DOC
beside x y = x <> text "" " " <> y

infixr 6 beside as <+>

below :: DOC -> DOC -> DOC
below x y = x <> line <> y

infixr 5 below as </>

folddoc :: (DOC -> DOC -> DOC) -> List DOC -> DOC
folddoc f List.Nil = nil
folddoc f (Cons x List.Nil) = x
folddoc f (Cons x xs) = f x $ folddoc f xs

spread :: List DOC -> DOC
spread = folddoc (<+>)

stack :: List DOC -> DOC
stack = folddoc (</>)

bracket' :: Int -> String -> DOC -> String -> DOC
bracket' i l x r = group $ text "" l <> nest i (line <> x) <> line <> text "" r

bracket :: String -> DOC -> String -> DOC
bracket = bracket' 2

besideOrBelow :: DOC -> DOC -> DOC
besideOrBelow x y = x <> (text "" " " :<|> line) <> y

infixr 6 besideOrBelow as <+/>

words :: String -> List String
words = List.fromFoldable <<< String.split (Pattern " ")

fillwords :: String -> DOC
fillwords = folddoc (<+/>) <<< map (text "") <<< words

fill :: List DOC -> DOC
fill List.Nil = nil
fill (Cons x List.Nil) = x
fill (Cons x (Cons y zs)) =
  (flatten x <+> fill (flatten y : zs))
  :<|>
  (x </> fill (y : zs))