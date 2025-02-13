module Foliage.Example.Ex2 where

import Foliage.Grammar
import Prelude

import Control.Monad.Error.Class (throwError)
import Data.List as List
import Data.Tuple.Nested ((/\))
import Foliage.Pretty (pretty)

prog :: Prog
prog = Prog $ List.fromFoldable
  [ DefRel relEven IntLat
  , DefRule ruleEven0 $ Rule
      { hyps: List.fromFoldable []
      , prop: Prop (Rel relEven) (DataTerm (IntTerm 0))
      }
  , DefRule ruleEvenS $ Rule
      { hyps: List.fromFoldable
          [ PropHyp $ Prop (Rel relOdd) (VarTerm x)
          , CompHyp x' $ Invoke funSuc [ VarTerm x ]
          ]
      , prop: Prop (Rel relEven) (VarTerm x')
      }

  , DefRel relOdd IntLat
  , DefRule ruleOddS $ Rule
      { hyps: List.fromFoldable
          [ PropHyp $ Prop (Rel relEven) (VarTerm x)
          , CompHyp x' $ Invoke funSuc [ VarTerm x ]
          ]
      , prop: Prop (Rel relOdd) (VarTerm x')
      }

  , DefRel relR $
      ProdLat LexicographicProdLatOrdering IntLat IntLat
  , DefRule ruleR0 $ Rule
      { hyps: List.fromFoldable []
      , prop:
          Prop (Rel relR) (PairTerm (DataTerm (IntTerm 0)) (DataTerm (IntTerm 0)))
      }
  , DefRule ruleR1 $ Rule
      { hyps: List.fromFoldable
          [ PropHyp $ Prop (Rel relR) (PairTerm (VarTerm x) (VarTerm y))
          , CompHyp y' (Invoke funSuc [ VarTerm y ])
          ]
      , prop:
          Prop (Rel relR) (PairTerm (VarTerm x) (VarTerm y'))
      }
  , DefRule ruleR2 $ Rule
      { hyps: List.fromFoldable
          [ PropHyp $ Prop (Rel relR) (PairTerm (VarTerm x) (VarTerm y))
          , PropHyp $ Prop (Rel (relOdd)) (VarTerm y)
          , CompHyp x' (Invoke funSuc [ VarTerm x ])
          ]
      , prop:
          Prop (Rel relR) (PairTerm (VarTerm x') (DataTerm (IntTerm 0)))
      }

  , DefFun funFst [ p /\ ProdTyp IntTyp IntTyp ] IntTyp case _ of
      [ PairTerm x _ ] -> pure x
      _ -> throwError "invalid args"
      args -> throwError $ "invalid args: " <> pretty args
  , DefFun funSnd [ p /\ ProdTyp IntTyp IntTyp ] IntTyp case _ of
      [ PairTerm _ y ] -> pure y
      _ -> throwError "invalid args"
      args -> throwError $ "invalid args: " <> pretty args
  , DefFun funSuc [ x /\ IntTyp ] IntTyp case _ of
      [ DataTerm (IntTerm x) ] -> pure $ DataTerm (IntTerm (x + 1))
      args -> throwError $ "invalid args: " <> pretty args
  ]
  where
  relEven = Name "Even"
  ruleEven0 = Name "Even0"
  ruleEvenS = Name "EvenS"

  relOdd = Name "Odd"
  ruleOddS = Name "OddS"

  relR = Name "R"
  ruleR0 = Name "R0"
  ruleR1 = Name "R1"
  ruleR2 = Name "R2"

  funSuc = Name "suc"
  funSnd = Name "snd"
  funFst = Name "fst"

  p = Name "p"
  x = Name "x"
  x' = Name "x'"
  y = Name "y"
  y' = Name "y'"
