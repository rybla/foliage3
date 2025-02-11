module Foliage.Example.Ex1 where

import Foliage.Grammar
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.List as List
import Data.Tuple.Nested ((/\))

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
    , DefRule (Name "R1") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relP) $ DataTerm (IntTerm 1) ]
        , prop: Prop (Rel relQ) $ DataTerm (IntTerm 1)
        }
    , DefFun funSuc [ Name "x" /\ IntType ] IntType case _ of
        [ DataTerm (IntTerm x) ] -> pure $ DataTerm (IntTerm (x + 1))
        _ -> throwError "invalid args"
    , DefRule (Name "R2") $ Rule
        { hyps: List.fromFoldable
            [ PropHyp $ Prop (Rel relQ) $ VarTerm (Name "x")
            , CompHyp (Name "x'") (Invoke funSuc [ VarTerm (Name "x") ])
            ]
        , prop: Prop (Rel relQ) $ VarTerm (Name "x'")
        }
    ]
  where
  relP = Name "P"
  relQ = Name "Q"
  funSuc = Name "Suc"
