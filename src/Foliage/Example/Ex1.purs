module Foliage.Example.Ex1 where

import Foliage.Grammar
import Prelude

import Data.List as List

prog :: Prog
prog =
  Prog $ List.fromFoldable
    [ DefRel relP IntLat
    , DefRule (Name "P1") $ Rule
        { hyps: List.fromFoldable []
        , prop: Prop (Rel relP) $ DataTerm (IntTerm 0)
        }
    , DefRule (Name "P2") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (IntTerm 0) ]
        , prop: Prop (Rel relP) $ DataTerm (IntTerm 1)
        }
    , DefRel relQ IntLat
    -- , DefRule (Name "R1") $ Rule
    --     { hyps: List.fromFoldable
    --         [ PropHyp $ Prop (Rel relP) $ DataTerm (IntTerm 1) ]
    --     , prop: Prop (Rel relQ) $ DataTerm (IntTerm 1)
    --     }
    ]
  where
  relP = Name "P"
  relQ = Name "Q"
