module Foliage.Ui.Console where

import Prelude

import Control.Monad.Writer (tell)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens ((%=))
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Effect.Aff.Class (class MonadAff)
import Foliage.Ui.Common (ConsoleQuery(..), Message)
import Foliage.Utility (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Type.Prelude (Proxy(..))

type State =
  { messages :: List Message
  }

component :: forall input output m. MonadAff m => H.Component ConsoleQuery input output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> State
  initialState _ =
    { messages: none }

  eval = H.mkEval H.defaultEval
    { handleQuery = handleQuery }

  handleQuery :: forall a. ConsoleQuery a -> _ (Maybe a)
  handleQuery (TellMessage msg a) = do
    prop (Proxy @"messages") %= List.Cons msg
    pure (Just a)

  render state =
    HH.div
      [ css do
          tell [ "flex-grow: 0", "flex-shrink: 1", "width: 50%" ]
          tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "padding: 0.5em" ]
      ]
      [ HH.div
          [ css do tell [ "padding: 0.5em", "background-color: black", "color: white" ] ]
          [ HH.text "console" ]
      , HHK.div
          [ css do
              tell [ "display: flex", "flex-direction: column-reverse", "gap: 0.5em" ]
              tell [ "overflow-y: scroll" ]
          ] $
          state.messages # foldMapWithIndex \i html ->
            [ Tuple (show i) $
                HH.div
                  [ css do tell [ "flex-shrink: 0", "padding: 0.5em", "border: 0.1em solid black" ] ]
                  [ html # HH.fromPlainHTML ]
            ]
      ]
