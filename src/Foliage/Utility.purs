module Foliage.Utility where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Data.Lens.Record as Data.Lens.Record
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Data.Variant
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[TODO]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[BUG]\n" <> msg

impossible :: forall a. Unit -> a
impossible _ = bug "impossible"

css :: forall r i. Writer (Array String) Unit -> HP.IProp (style :: String | r) i
css w = HP.style $ w # execWriter # Array.foldMap (_ <> "; ")

prop :: forall @l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => (forall p. Strong p => p a b -> p (Record r1) (Record r2))
prop = Data.Lens.Record.prop (Proxy @l)

inj :: forall @sym a r1 r2. Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inj = Data.Variant.inj (Proxy @sym)

bind' :: forall m a b. Bind m => (a -> m b) -> m a -> m b
bind' = bindFlipped

mapMap :: forall f33 f36 a37 b38. Functor f33 => Functor f36 => (a37 -> b38) -> f33 (f36 a37) -> f33 (f36 b38)
mapMap f = map (map f)

infixl 4 mapMap as <$$>