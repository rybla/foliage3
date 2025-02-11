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
            [ PropHyp $ Prop (Rel relP) $ VarTerm $ Name "n"
            , CompHyp (Name "b") $ Invoke (Name "isNonezero") [ VarTerm $ Name "n" ]
            , CondHyp $ VarTerm $ Name "b"
            ]
        , prop: Prop (Rel relP) $ DataTerm (NatTerm 4)
        }
    ]
  where
  relP = Name "P"
  ruleR1 = Name "R1"
