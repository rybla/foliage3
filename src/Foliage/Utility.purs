module Foliage.Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[TODO]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[BUG]\n" <> msg

