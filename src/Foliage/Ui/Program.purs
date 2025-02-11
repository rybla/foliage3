module Foliage.Ui.Program where

import Foliage.Grammar
import Prelude

import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe, maybe)
import Data.Unfoldable (none)
import Foliage.Ui.Common (Message)
import Foliage.Utility (css)
import Halogen as H
import Halogen.HTML as HH

type Input =
  { prog :: Prog
  }

type Output = Message

type State =
  { prog :: Prog
  }

data Action = Raise Output

component :: forall query m. Monad m => H.Component query Input Output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState input =
    { prog: input.prog }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction (Raise o) = do
    H.raise o

  render state =
    HH.div
      [ css do
          tell [ "flex-grow: 1", "flex-shrink: 1" ]
          tell [ "padding: 0.5em" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ css do tell [ "padding: 0.5em", "background-color: black", "color: white" ] ]
          [ HH.text "program" ]
      , HH.div
          [ css do
              tell [ "width: calc(100% - 2*0.5em)", "padding: 0.5em", "font-family: monospace" ]
              tell [ "height: calc(100vh - 3em)", "overflow-y: scroll" ]
          ]
          [ renderProg state.prog ]
      ]

type HTML m = HH.ComponentHTML Action () m

renderProg :: forall m. Monad m => Prog -> HTML m
renderProg (Prog stmts) =
  HH.div
    [ css do
        -- tell [ "gap: 0.5em" ]
        tell [ "display: flex", "flex-direction: column", "width: 100%" ]
    ] $
    stmts # foldMap (renderStmt >>> pure)

renderStmt :: forall m. Monad m => Stmt -> HTML m
renderStmt (DefRel x l) =
  renderStmt_template
    { labels:
        [ HH.div [] [ renderPunc "rel" ]
        , HH.div [] [ renderLine $ fold $ [ [ renderName x, renderPunc "(" ], renderLat l, [ renderPunc ")" ] ] ]
        ]
    , body: none
    }
renderStmt (DefRule x r) =
  renderStmt_template
    { labels:
        [ HH.div [] [ renderPunc "rule" ]
        , HH.div [] [ renderName x ]
        ]
    , body: pure $
        HH.div [] [ renderRule r ]
    }

renderStmt_template :: forall m. Monad m => { labels :: Array (HTML m), body :: Maybe (HTML m) } -> HTML m
renderStmt_template { labels, body } =
  HH.div
    [ css do
        -- tell ["box-shadow: 0 0 0 1px black inset"]
        -- tell [ "width: 100%", "overflow: scroll" ]
        tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "padding: 0.5em" ]
    ] $ fold
    [ [ HH.div
          [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
          labels
      ]
    , body # maybe none \body ->
        [ HH.div [ css do tell [ "margin-left: 1em" ] ]
            [ body ]
        ]
    ]

renderRule :: forall m. Monad m => Rule -> HTML m
renderRule (Rule rule) =
  HH.div
    [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ] $ fold
    [ rule.hyps # foldMap (renderHyp >>> pure)
    , [ HH.hr [ css do tell [ "border: none", "height: 0.2em", "background-color: black" ] ] ]
    , [ renderProp rule.prop ]
    ]

renderHyp :: forall m. Monad m => Hyp -> HTML m
renderHyp (PropHyp p) = renderProp p
renderHyp (CompHyp x c) = renderLine [ renderName x, renderPunc "<-", renderComp c ]
renderHyp (CondHyp a) = renderLine [ renderPunc "if", renderTerm a ]

renderComp :: forall m. Monad m => Comp -> HTML m
renderComp (Invoke x args) = renderLine $ fold $
  [ [ renderName x, renderPunc "(" ]
  , args # map renderTerm # Array.intersperse (renderPunc ",")
  , [ renderPunc ")" ]
  ]

renderProp :: forall m. Monad m => Prop -> HTML m
renderProp (Prop r a) = renderLine [ renderRel r, renderPunc "(", renderTerm a, renderPunc ")" ]

renderRel :: forall m. Monad m => Rel -> HTML m
renderRel (Rel x) = renderName x

renderLat :: forall m. Monad m => Lat -> Array (HTML m)
renderLat UnitLat = [ renderKeyword "Unit" ]
renderLat NatLat = [ renderKeyword "Nat" ]
renderLat (DiscreteLat l) = [ renderKeyword "Discrete", renderPunc "(" ] <> renderLat l <> [ renderPunc ")" ]
renderLat (OppositeLat l) = [ renderKeyword "Opposite", renderPunc "(" ] <> renderLat l <> [ renderPunc ")" ]

renderTerm :: forall m. Monad m => Term -> HTML m
renderTerm UnitTerm = renderKeyword "unit"
renderTerm (NatTerm n) = renderKeyword (show n)
renderTerm (VarTerm x) = renderName x

renderName :: forall m. Monad m => Name -> HTML m
renderName x =
  HH.div
    [ css do tell [ "color: blue", "text-decoration: underline" ] ]
    [ HH.text $ fromNameToString x ]

renderKeyword :: forall m. Monad m => String -> HTML m
renderKeyword s =
  HH.div
    [ css do tell [ "color: green" ] ]
    [ HH.text s ]

renderPunc :: forall m. Monad m => String -> HTML m
renderPunc s =
  HH.div
    [ css do tell [ "color: black" ] ]
    [ HH.text s ]

renderLine :: forall m. Monad m => Array (HTML m) -> HTML m
renderLine xs =
  HH.div
    [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
    xs
