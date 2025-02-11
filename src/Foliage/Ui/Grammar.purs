module Foliage.Ui.Grammar where

import Foliage.Grammar
import Prelude

import Control.Monad.Reader (Reader, ask)
import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Foldable (fold, foldMap, intercalate, null)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Data.Variant (Variant)
import Foliage.Pretty (pretty)
import Foliage.Utility (css)
import Halogen.HTML as HH

type HTML m as = HH.ComponentHTML (Variant as) () m

type Ctx =
  { props :: List Prop
  }

renderProg :: forall m as. Prog -> Reader Ctx (HTML m as)
renderProg (Prog stmts) =
  HH.div
    [ css do
        tell [ "gap: 0.5em" ]
        tell [ "display: flex", "flex-direction: column", "width: 100%" ]
    ] <$>
    (stmts # traverse renderStmt # map Array.fromFoldable)

renderStmt :: forall m as. Stmt -> Reader Ctx (HTML m as)
renderStmt (DefRel x l) = do
  ctx <- ask
  labels <-
    [ HH.div [] <$> ([ renderPunc "relation" ] # sequence)
    , HH.div [] <$>
        ( [ renderLine =<<
              ( [ [ renderName x, renderPunc "(" ] # sequence
                , renderLat l
                , [ renderPunc ")" ] # sequence
                ] # fold
              )
          ] # sequence
        )
    ] # sequence
  htmls_props <- ctx.props
    # foldMap
        ( \prop@(Prop (Rel x') _) ->
            if x /= x' then []
            else
              prop # renderProp # map (\html -> HH.li [] [ html ]) # pure
        )
    # sequence
  body <-
    if null htmls_props then
      pure none
    else pure
      <$> HH.div
        [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
      <$>
        ( [ renderPunc "known instances:"
          , pure $
              HH.ul
                [ css do tell [ "padding-left: 1em" ] ]
                htmls_props
          ] # sequence
        )
  renderStmt_template { labels, body }
renderStmt (DefRule x r) = do
  labels <-
    [ HH.div [] <$> ([ renderPunc "rule" ] # sequence)
    , HH.div [] <$> ([ renderName x ] # sequence)
    ] # sequence
  body <-
    HH.div [] <$>
      ([ renderRule r ] # sequence)
  renderStmt_template { labels, body: pure body }
renderStmt (DefFun x ps t f) = do
  labels <-
    [ HH.div [] <$> ([ renderPunc "fun" ] # sequence)
    , HH.div
        [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ] <$>
        ( [ [ renderName x ]
          , [ renderPunc "(" ]
          , ps
              # map (\(x' /\ t') -> [ renderName x', renderPunc ":", renderTyp t' ])
              # intercalate [ renderPunc "," ]
          , [ renderPunc ")" ]
          , [ renderPunc "→", renderTyp t ]
          , [ renderPunc ":=", renderPunc "<fun>" ]
          ] # fold # sequence
        )
    ] # sequence
  renderStmt_template { labels, body: none }

renderTyp :: forall m as. Typ -> Reader Ctx (HTML m as)
renderTyp UnitTyp = renderKeyword "𝕌"
renderTyp BoolType = renderKeyword "𝔹"
renderTyp IntType = renderKeyword "ℤ"

renderStmt_template :: forall m as. { labels :: Array (HTML m as), body :: Maybe (HTML m as) } -> Reader Ctx (HTML m as)
renderStmt_template { labels, body } =
  pure
    $ HH.div
        [ css do
            -- tell ["box-shadow: 0 0 0 1px black inset"]
            -- tell [ "width: 100%", "overflow: scroll" ]
            -- tell [ "border-radius: 1em", "box-shadow: -0.2em 0 0 0 black" ]
            tell [ "border-radius: 1em", "border-left: 0.2em solid black" ]
            tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "padding: 0.5em" ]
        ]
    $ fold
        [ [ HH.div
              [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
              labels
          ]
        , body # maybe none \body' ->
            [ HH.div [ css do tell [ "margin-left: 1em" ] ]
                [ body' ]
            ]
        ]

renderRule :: forall m as. Rule -> Reader Ctx (HTML m as)
renderRule (Rule rule) =
  HH.div
    [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ] <$>
    ( [ rule.hyps # foldMap (renderHyp >>> pure)
      , [ HH.div [ css do tell [ "height: 0.1em", "background-color: black" ] ] [] # pure ]
      , [ renderProp rule.prop ]
      ] # fold # sequence
    )

renderHyp :: forall m as. Hyp -> Reader Ctx (HTML m as)
renderHyp (PropHyp p) = renderProp p
renderHyp (CompHyp x c) = renderLine =<< sequence [ renderName x, renderPunc "←", renderComp c ]
renderHyp (CondHyp a) = renderLine =<< sequence [ renderPunc "if", renderTerm a ]

renderComp :: forall m as. Comp -> Reader Ctx (HTML m as)
renderComp (Invoke x args) =
  renderLine =<<
    ( [ [ renderName x, renderPunc "(" ]
      , args # map renderTerm # Array.intersperse (renderPunc ",")
      , [ renderPunc ")" ]
      ] # fold # sequence
    )

renderProp :: forall m as. Prop -> Reader Ctx (HTML m as)
renderProp (Prop r a) = renderLine =<< sequence [ renderRel r, renderPunc "(", renderTerm a, renderPunc ")" ]

renderRel :: forall m as. Rel -> Reader Ctx (HTML m as)
renderRel (Rel x) = renderName x

renderLat :: forall m as. Lat -> Reader Ctx (Array (HTML m as))
renderLat UnitLat = renderKeyword "Unit" # map pure
renderLat BoolLat = renderKeyword "Bool" # map pure
renderLat IntLat = renderKeyword "Int" # map pure
renderLat (DiscreteLat l) =
  [ [ renderKeyword "Discrete", renderPunc "(" ] # sequence
  , renderLat l
  , [ renderPunc ")" ] # sequence
  ] # sequence # map fold
renderLat (OppositeLat l) =
  [ [ renderKeyword "Opposite", renderPunc "(" ] # sequence
  , renderLat l
  , [ renderPunc ")" ] # sequence
  ] # sequence # map fold

renderTerm :: forall m as. Term -> Reader Ctx (HTML m as)
renderTerm (DataTerm UnitTerm) = renderKeyword "unit"
renderTerm (DataTerm (BoolTerm b)) = renderKeyword (show b)
renderTerm (DataTerm (IntTerm n)) = renderKeyword (show n)
renderTerm (VarTerm x) = renderName x

renderName :: forall m as. Name -> Reader Ctx (HTML m as)
renderName x =
  pure
    $ HH.div
        [ css do tell [ "color: blue" ] ]
        [ HH.text $ pretty x ]

renderKeyword :: forall m as. String -> Reader Ctx (HTML m as)
renderKeyword s =
  pure
    $ HH.div
        [ css do tell [ "color: green" ] ]
        [ HH.text s ]

renderPunc :: forall m as. String -> Reader Ctx (HTML m as)
renderPunc s =
  pure $
    HH.div
      [ css do tell [ "color: black" ] ]
      [ HH.text s ]

renderLine :: forall m as. Array (HTML m as) -> Reader Ctx (HTML m as)
renderLine xs =
  pure
    $ HH.div
        [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
        xs
