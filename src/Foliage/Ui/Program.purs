module Foliage.Ui.Program where

import Foliage.Grammar
import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (tell)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect.Aff (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Foliage.Engine as Engine
import Foliage.Pretty (pretty)
import Foliage.Ui.Common (Message)
import Foliage.Ui.Grammar (renderProg)
import Foliage.Utility (css, inj)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE

type Input =
  { prog :: Prog
  }

type Output = Message

type State =
  { prog :: Prog
  , props :: List Prop
  , initial_gas :: Int
  , delay_duration :: Milliseconds
  , status :: Variant ("ready" :: Unit, "running" :: Unit)
  , result :: Maybe String
  }

component :: forall query m. MonadAff m => H.Component query Input Output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState input =
    { prog: input.prog
    , props: none
    , initial_gas: 10
    , delay_duration: 800.0 # wrap
    , status: inj @"ready" unit
    , result: none
    }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction = match
    { raise: \o -> do
        H.raise o
    , fixpoint: \_ -> do
        modify_ _
          { status = inj @"running" unit
          , result = none
          , props = none
          }
        state <- get
        Engine.main
          { prog: state.prog
          , initial_gas: state.initial_gas
          , delay_duration: state.delay_duration
          , set_props: \props -> modify_ _ { props = props }
          , trace: H.raise
          } # runExceptT >>= case _ of
          Left err -> do
            modify_ _
              { status = inj @"ready" unit
              , result = pure "error"
              }
            H.raise $
              HH.div
                []
                [ HH.div [] [ HH.text "error" ]
                , HH.div [] err
                ]
          Right result -> do
            modify_ _
              { status = inj @"ready" unit
              , result = pure "success"
              , props = result.props
              }
    }

  render state =
    let
      html_prog =
        renderProg state.prog
          # flip runReader
              { props: state.props }
    in
      HHK.div
        [ css do
            tell [ "flex-grow: 1", "flex-shrink: 1" ]
            tell [ "padding: 0.5em" ]
            tell [ "display: flex", "flex-direction: column" ]
        ]
        [ Tuple "title" $
            HH.div
              [ css do tell [ "padding: 0.5em", "background-color: black", "color: white" ] ]
              [ HH.text "program" ]
        , Tuple "controls" $
            state.status # match
              { ready: const
                  $ HH.div
                      [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row", "gap: 1em" ] ]
                  $ fold
                      [ [ HH.button
                            [ HE.onClick $ const $ inj @"fixpoint" unit ]
                            [ HH.text "fixpoint" ]
                        ]
                      , state.result # maybe [] \result ->
                          [ HH.div [] [ HH.text result ] ]
                      ]
              , running: const $
                  HH.div
                    [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row" ] ]
                    [ HH.text "running..." ]
              }
        , Tuple "params"
            let
              item_style = do
                tell [ "padding: 0.25em 0.5em", "border: 0.1em solid black", "border-radius: 1em" ]
            in
              HH.div
                [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row", "gap: 1em", "flex-wrap: wrap" ] ]
                [ HH.div [ css item_style ] [ HH.text $ "initial_gas = " <> show state.initial_gas ]
                , HH.div [ css item_style ] [ HH.text $ "delay_duration = " <> show state.delay_duration ]
                ]
        , Tuple "prog" $
            HH.div
              [ css do
                  tell [ "width: calc(100% - 2*0.5em)", "padding: 0.5em", "font-family: monospace" ]
                  tell [ "height: calc(100vh - 3em)", "overflow-y: scroll" ]
              ]
              [ html_prog ]
        ]

