module Foliage.Ui.Program where

import Foliage.Grammar
import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (tell)
import Data.Either (Either(..))
import Data.List (List)
import Data.Unfoldable (none)
import Data.Variant (match)
import Foliage.Engine as Engine
import Foliage.Ui.Common (Message)
import Foliage.Ui.Grammar (renderProg)
import Foliage.Utility (css, inj)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input =
  { prog :: Prog
  }

type Output = Message

type State =
  { prog :: Prog
  , props :: List Prop
  }

component :: forall query m. Monad m => H.Component query Input Output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState input =
    { prog: input.prog
    , props: none
    }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction = match
    { raise: \o -> do
        H.raise o
    , fixpoint: \_ -> do
        H.raise $ HH.text "handleAction.GenerateFixpoint"
        { prog } <- get
        Engine.main
          { prog
          , gas: 100
          , trace: H.raise
          } # runExceptT >>= case _ of
          Left err -> H.raise $
            HH.div
              []
              [ HH.div [] [ HH.text "error" ]
              , HH.div [] err
              ]
          Right result -> do
            modify_ _ { props = result.props }
    }

  -- handleAction (Raise o) = do
  --   H.raise o
  -- handleAction GenerateFixpoint = do
  --   H.raise $ HH.text "handleAction.GenerateFixpoint"
  --   { prog } <- get
  --   Engine.main
  --     { prog
  --     , gas: 100
  --     , trace: H.raise
  --     } # runExceptT >>= case _ of
  --     Left err -> H.raise $
  --       HH.div
  --         []
  --         [ HH.div [] [ HH.text "error" ]
  --         , HH.div [] err
  --         ]
  --     Right result -> do
  --       modify_ _ { props = result.props }

  render state =
    let
      html_prog =
        renderProg state.prog
          # flip runReader
              { props: state.props }
    in
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
            [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row" ] ]
            [ HH.button
                [ HE.onClick $ const $ inj @"fixpoint" unit ]
                [ HH.text "fixpoint" ]
            ]
        , HH.div
            [ css do
                tell [ "width: calc(100% - 2*0.5em)", "padding: 0.5em", "font-family: monospace" ]
                tell [ "height: calc(100vh - 3em)", "overflow-y: scroll" ]
            ]
            [ html_prog ]
        ]

