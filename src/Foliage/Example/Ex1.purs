module Foliage.Example.Ex1 where

import Foliage.Grammar
import Prelude

import Data.List as List

prog :: Prog
prog =
  Prog $ List.fromFoldable
    [ DefRel relP NatLat
    , DefRule ruleR1 $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (NatTerm 0) ]
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 1)
        }
    ]
  where
  relP = Name "P"
  ruleR1 = Name "R1"
