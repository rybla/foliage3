module Foliage.Ui.Main where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Foliage.Ui.Common (ConsoleQuery(..), Message)
import Foliage.Ui.Console as Ui.Console
import Foliage.Ui.Program as Ui.Program
import Foliage.Utility (css)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component {} =<< HA.awaitBody)

data Action = MessageAction { label :: String, content :: Message }

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction (MessageAction msg) = H.tell (Proxy @"console") unit $ TellMessage msg

  render _ =
    HH.div
      [ css do tell [ "display: flex", "flex-direction: row", "height: 100vh", "width: 100vw" ] ]
      [ HH.slot (Proxy @"program") unit Ui.Program.component {} MessageAction
      , HH.slot (Proxy @"console") unit Ui.Console.component {} absurd
      ]

