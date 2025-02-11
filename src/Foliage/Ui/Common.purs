module Foliage.Ui.Common where

import Prelude

import Halogen.HTML (PlainHTML)

data ConsoleQuery a = TellMessage Message a

type Message = PlainHTML

type Error = Array PlainHTML