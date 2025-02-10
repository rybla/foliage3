module Foliage.Ui.Program where

import Prelude

import Control.Monad.Writer (tell)
import Data.Lens ((+=))
import Data.Lens.Record (prop)
import Effect.Aff.Class (class MonadAff)
import Foliage.Ui.Common (Message)
import Foliage.Utility (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type Output = Message

data Action = Raise Output

component :: forall query input m. MonadAff m => H.Component query input Output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { counter: 0
    }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction (Raise o) = do
    prop (Proxy @"counter") += 1
    H.raise o

  render state =
    HH.div
      [ css do
          tell [ "flex-grow: 1", "flex-shrink: 1" ]
          tell [ "padding: 0.5em" ]
      ]
      [ HH.div
          [ css do tell [ "padding: 0.5em", "background-color: black", "color: white" ] ]
          [ HH.text "program" ]
      , HH.div [] [ HH.text $ show state.counter ]
      , HH.button
          [ HE.onClick $ const $ Raise $ HH.text $ "test message #" <> show state.counter ]
          [ HH.text "raise message" ]
      ]
