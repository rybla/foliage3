module Foliage.Ui.Program where

import Foliage.Grammar
import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.State (gets, put)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.Array (intercalate, intersperse)
import Data.Array as Array
import Data.Either (Either(..), fromRight')
import Data.Foldable (fold)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe', maybe, maybe')
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Foliage.Engine as Engine
import Foliage.Example.Ex1 as Ex1
import Foliage.Example.Ex2 as Ex2
import Foliage.Ui.Common (Message, Error)
import Foliage.Ui.Grammar as Ui.Grammar
import Foliage.Utility (css, impossible, inj)
import Halogen (HalogenM, get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  {}

type Output = { label :: String, content :: Message }

type State =
  { example_name :: String
  , prog :: Prog
  , props :: List Prop
  , initial_gas :: Int
  , delay_duration :: Milliseconds
  , status :: Variant ("ready" :: Unit, "running" :: Unit)
  , result :: Maybe Error
  , stopped :: Boolean
  }

example_progs :: Array (String /\ Prog)
example_progs =
  [ "ex1" /\ Ex1.prog
  , "ex2" /\ Ex2.prog
  ]

component :: forall query m. MonadAff m => H.Component query Input Output m
component = H.mkComponent { initialState, eval, render }
  where
  initial_example_name = "ex2"

  initialState :: Input -> State
  initialState input =
    { example_name: initial_example_name
    , prog: example_progs # Array.find (fst >>> (_ == initial_example_name)) # maybe' impossible snd
    , props: none
    , initial_gas: 10
    -- , delay_duration: 800.0 # wrap
    , delay_duration: 0.0 # wrap
    , status: inj @"ready" unit
    , result: none
    , stopped: false
    }

  eval = H.mkEval H.defaultEval
    { receive = inj @"receive" >>> pure
    , handleAction = handleAction
    }

  handleAction = match
    { receive: initialState >>> put
    , raise: \o -> do
        H.raise o
    , fixpoint: \_ -> do
        modify_ _
          { status = inj @"running" unit
          , result = none
          , props = none
          , stopped = false
          }
        state <- get
        result <-
          Engine.main
            { prog: state.prog
            , initial_gas: state.initial_gas
            , delay_duration: state.delay_duration
            , set_props: \props -> modify_ _ { props = props }
            -- , trace: make_trace none 
            , trace: make_trace $ pure [ "applyRule" ]
            , stopped: gets _.stopped
            } # runExceptT
        case result of
          Left err -> do
            modify_ _
              { status = inj @"ready" unit
              , result = pure err
              }
            H.raise $
              { label: "error"
              , content:
                  HH.div
                    []
                    [ HH.div [] [ HH.text "error" ]
                    , HH.div [ css do tell [ "display: flex", "flex-direction: column" ] ] err
                    ]
              }
          Right { props } -> do
            modify_ _
              { status = inj @"ready" unit
              , result = pure [ HH.text "success" ]
              , props = props
              }
    , stop: \_ -> do
        modify_ _ { stopped = true }
    , set_delay_duration: \delay_duration -> modify_ _ { delay_duration = delay_duration # wrap }
    , set_initial_gas: \initial_gas -> modify_ _ { initial_gas = initial_gas }
    , set_example: \k -> do
        let prog = example_progs # Array.find (fst >>> (_ == k)) # maybe' impossible snd
        modify_ _ { prog = prog }
    }

  render state =
    let
      html_prog =
        Ui.Grammar.prog state.prog
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
              [ css do tell [ "padding: 0.5em", "background-color: black", "color: white", "display: flex", "flex-direction: row", "gap: 1em", "justify-content: space-between" ] ]
              [ HH.div [] [ HH.text "program" ]
              , HH.div
                  [ css do tell [ "display: flex", "flex-direction: row", "gap: 1em" ] ]
                  [ HH.select
                      [ HE.onValueChange $ inj @"set_example"
                      , HP.value state.example_name
                      ] $
                      example_progs # map \(k /\ _) ->
                        HH.option [ HP.value k ] [ HH.text k ]
                  ]
              ]
        , Tuple "controls" $
            state.status # match
              { ready: const
                  $ HH.div
                      [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row", "gap: 1em" ] ]
                  $ fold
                      [ [ HH.div []
                            [ HH.button
                                [ HE.onClick $ const $ inj @"fixpoint" unit
                                , css do tell [ "flex-grow: 0", "flex-shrink: 0" ]
                                ]
                                [ HH.text "fixpoint" ]
                            ]
                        ]
                      , state.result # maybe [] \result ->
                          [ HH.div
                              -- "flex-direction: row", "gap: 0.5em", "flex-wrap: wrap"
                              [ css do tell [ "flex-grow: 1", "flex-shrink: 1", "display: flex" ] ]
                              (result # map HH.fromPlainHTML # intersperse (HH.text " | "))
                          ]
                      ]
              , running: const $
                  HH.div
                    [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
                    [ HH.div [] [ HH.text "running..." ]
                    , HH.div [] [ HH.button [ HE.onClick $ const $ inj @"stop" unit ] [ HH.text "stop" ] ]
                    ]
              }
        , Tuple "params" $
            HH.div
              [ css do tell [ "padding: 0.5em", "display: flex", "flex-direction: row", "gap: 1em", "flex-wrap: wrap" ] ]
              [ HH.div
                  [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em", "align-items: center" ] ]
                  [ HH.div [] [ HH.text "initial_gas =" ]
                  , HH.select
                      [ HE.onValueChange \s -> inj @"set_initial_gas" (s # fromJsonString # fromRight' impossible)
                      , HP.value $ toJsonString state.initial_gas
                      ] $
                      [ 10, 100 ] # map \v ->
                        HH.option [ HP.value $ toJsonString v ] [ HH.text $ show v <> "steps" ]
                  ]
              , HH.div
                  [ css do tell [ "display: flex", "flex-direction: row", "gap: 0.5em", "align-items: center" ] ]
                  [ HH.div [] [ HH.text "delay_duration =" ]
                  , HH.select
                      [ HE.onValueChange \s -> inj @"set_delay_duration" (s # fromJsonString # fromRight' impossible)
                      , HP.value $ toJsonString $ unwrap state.delay_duration
                      ] $
                      [ 0.0, 100.0, 500.0, 800.0, 1000.0 ] # map \v ->
                        HH.option [ HP.value $ toJsonString v ] [ HH.text $ show v <> "ms" ]
                  ]
              ]
        , Tuple "prog" $
            HH.div
              [ css do
                  tell [ "width: calc(100% - 2*0.5em)", "padding: 0.5em", "font-family: monospace" ]
                  tell [ "height: calc(100vh - 3em)", "overflow-y: scroll" ]
              ]
              [ html_prog ]
        ]

make_trace :: forall content state action slots m label. Eq label => Maybe (Array label) -> label -> content -> HalogenM state action slots { content :: content, label :: label } m Unit
make_trace Nothing label content = H.raise { label, content }
make_trace (Just labels) label content = when (label `Array.elem` labels) do H.raise { label, content }
