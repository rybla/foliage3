module Foliage.Utility where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[TODO]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[BUG]\n" <> msg

css :: forall r i. Writer (Array String) Unit -> HP.IProp (style :: String | r) i
css w = HP.style $ w # execWriter # Array.foldMap (_ <> "; ")

