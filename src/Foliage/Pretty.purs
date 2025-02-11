module Foliage.Pretty where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate, null)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Boolean where
  pretty = show

instance Pretty a => Pretty (List a) where
  pretty xs | null xs = "∅"
  pretty xs = xs # map pretty # intercalate ", "

instance Pretty a => Pretty (Array a) where
  pretty xs | null xs = "∅"
  pretty xs = xs # map pretty # intercalate ", "

instance (Pretty a, Pretty b) => Pretty (Tuple a b) where
  pretty (x /\ y) = pretty x <> " /\\ " <> pretty y

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = pretty a
  pretty (Right a) = pretty a

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "∅"
  pretty (Just x) = pretty x

parens :: String -> String
parens = surround "(" ")"

brackets :: String -> String
brackets = surround "[" "]"

braces :: String -> String
braces = surround "{" "}"

surround :: forall m. Semigroup m => m -> m -> m -> m
surround start end = (start <> _) <<< (_ <> end)