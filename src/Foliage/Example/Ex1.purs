module Foliage.Example.Ex1 where

import Foliage.Grammar
import Prelude

import Data.List as List

prog :: Prog
prog =
  Prog $ List.fromFoldable
    [ DefRel relP NatLat
    , DefRule (Name "P1") $ Rule
        { hyps: List.fromFoldable []
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 0)
        }
    , DefRule (Name "P2") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (NatTerm 0) ]
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 1)
        }
    , DefRel relQ NatLat
    , DefRule (Name "R1") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (NatTerm 1) ]
        , prop: Prop (Rel relQ) $ DataTerm (NatTerm 1)
        }
    ]
  where
  relP = Name "P"
  relQ = Name "Q"
