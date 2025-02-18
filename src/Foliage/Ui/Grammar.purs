module Foliage.Ui.Grammar where

import Foliage.Grammar
import Prelude

import Control.Applicative (pure)
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
import Foliage.Utility (bind', css)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type HTML m as = HH.ComponentHTML (Variant as) () m

type Ctx =
  { props :: List Prop
  }

prog :: forall m as. Prog -> Reader Ctx (HTML m as)
prog (Prog stmts) =
  HH.div
    [ css do
        tell [ "gap: 0.5em" ]
        tell [ "display: flex", "flex-direction: column", "width: 100%" ]
    ] <$>
    (stmts # traverse stmt # map Array.fromFoldable)

stmt :: forall m as. Stmt -> Reader Ctx (HTML m as)
stmt (DefRel x l) = do
  ctx <- ask
  label <- [ punc "relation", name x, punc "[", lat l, punc "]" ] # fold # bind' line
  body <-
    let
      m_props = ctx.props # foldMap case _ of
        p@(Prop (Rel x') _) | x == x' ->
          [ HH.div [] <$>
              ( [ ( append
                      <$> pure [ HH.div [ css do tell [ "padding-left: 0.5em" ] ] [ HH.text "•" ] ]
                      <*> prop p
                  ) >>= line
                ] # sequence
              )
          ]
        _ -> []
    in
      if null m_props then pure none
      else do
        props <- m_props # sequence
        pure
          <$> HH.div
            [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
          <$>
            ( [ punc "known instances:"
              , pure
                  [ HH.div
                      [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
                      props
                  ]
              ] # fold
            )

  stmt_template { label, body }
stmt (DefRule x r) = do
  label <- [ punc "rule", name x ] # fold # bind' line
  body <- rule r
  stmt_template { label, body: pure body }
stmt (DefFun x ps t _) = do
  let params = ps # map (\(x' /\ t') -> [ name x', punc ":", typ t' ]) # intercalate [ punc "," ] # fold
  label <- [ punc "fun", name x, punc "(", params, punc ")", punc "→", typ t, punc ":=", punc "<fun>" ] # fold # bind' line
  stmt_template { label, body: none }

typ :: forall m as. Typ -> Reader Ctx (Array (HTML m as))
typ UnitTyp = kw "𝕌"
typ BoolTyp = kw "𝔹"
typ IntTyp = kw "ℤ"
typ (ProdTyp a b) = [ punc "(", typ a, kw "×", typ b, punc ")" ] # fold

stmt_template :: forall m as. { label :: HTML m as, body :: Maybe (HTML m as) } -> Reader Ctx (HTML m as)
stmt_template { label, body } =
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
        [ [ label ]
        , body # maybe none \body' ->
            [ HH.div [ css do tell [ "margin-left: 1em" ] ]
                [ body' ]
            ]
        ]

rule :: forall m as. Rule -> Reader Ctx (HTML m as)
rule (Rule r) =
  HH.div
    [ css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ] <$>
    ( [ r.hyps # foldMap (\h -> [ hyp h >>= line ] # sequence)
      , [ HH.div [ css do tell [ "height: 0.1em", "background-color: black" ] ] [] ] # pure
      , [ prop r.prop >>= line ] # sequence
      ] # fold
    )

hyp :: forall m as. Hyp -> Reader Ctx (Array (HTML m as))
hyp (PropHyp p) = prop p
hyp (CompHyp x c) = [ name x, punc "←", comp c ] # fold
hyp (CondHyp a) = [ punc "if", term a ] # fold

comp :: forall m as. Comp -> Reader Ctx (Array (HTML m as))
comp (Invoke x args) = [ name x, punc "(", args # map term # intercalate (punc ","), punc ")" ] # fold

prop :: forall m as. Prop -> Reader Ctx (Array (HTML m as))
prop (Prop r a) = [ rel r, punc "[", term a, punc "]" ] # fold

rel :: forall m as. Rel -> Reader Ctx (Array (HTML m as))
rel (Rel x) = name x

lat :: forall m as. Lat -> Reader Ctx (Array (HTML m as))
lat UnitLat = kw "Unit"
lat BoolLat = kw "Bool"
lat IntLat = kw "Int"
lat (ProdLat o a b) = [ punc "(", lat a, [ kw "×", sub (prodLatOrdering o) ] # fold # bind' span, lat b, punc ")" ] # fold
lat (DiscreteLat l) = [ kw "Discrete", punc "(", lat l, punc ")" ] # fold
lat (OppositeLat l) = [ kw "Opposite", punc "(", lat l, punc ")" ] # fold

prodLatOrdering :: forall m as. ProdLatOrdering -> Reader Ctx (Array (HTML m as))
prodLatOrdering LexicographicProdLatOrdering = kw "lex"

term :: forall m as. Term -> Reader Ctx (Array (HTML m as))
term (VarTerm x) = name x
term (DataTerm UnitTerm) = kw "unit"
term (DataTerm (BoolTerm b)) = kw (show b)
term (DataTerm (IntTerm n)) = kw (show n)
term (PairTerm a b) = [ punc "(", term a, punc " , ", term b, punc ")" ] # fold

name :: forall m as. Name -> Reader Ctx (Array (HTML m as))
name x =
  pure
    [ HH.div
        [ css do tell [ "color: blue" ] ]
        [ HH.text $ pretty x ]
    ]

kw :: forall m as. String -> Reader Ctx (Array (HTML m as))
kw s =
  pure
    [ HH.div
        [ css do tell [ "color: green" ] ]
        [ HH.text s ]
    ]

punc :: forall m as. String -> Reader Ctx (Array (HTML m as))
punc s =
  pure
    [ HH.div
        [ css do tell [ "color: black" ] ]
        [ HH.text s ]
    ]

line :: forall m as. Array (HTML m as) -> Reader Ctx (HTML m as)
line xs =
  pure $
    HH.div
      [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
      xs

span :: forall m as. Array (HTML m as) -> Reader Ctx (Array (HTML m as))
span = HH.span [ HP.classes [ HH.ClassName "inline-kids" ] ] >>> pure >>> pure

sub :: forall m as. Reader Ctx (Array (HTML m as)) -> Reader Ctx (Array (HTML m as))
sub = map \htmls -> [ HH.sub_ htmls ]
