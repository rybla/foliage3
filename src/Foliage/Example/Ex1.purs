module Foliage.Example.Ex1 where

import Foliage.Grammar
import Prelude

import Data.List as List

prog :: Prog
prog =
  Prog $ List.fromFoldable
    [ DefRel relP NatLat
    , DefRule (Name "P(0)") $ Rule
        { hyps: List.fromFoldable []
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 0)
        }
    , DefRule (Name "P(0) => P(1)") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (NatTerm 0) ]
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 1)
        }
    ]
  where
  relP = Name "P"
