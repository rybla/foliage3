module Foliage.Engine where

import Foliage.Grammar
import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (StateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldM)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (fold, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Unfoldable1 (singleton)
import Effect.Class (class MonadEffect)
import Foliage.Utility (todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M m = ExceptT Error (ReaderT (Ctx m) (StateT Env m))

type Ctx m =
  { relations :: Map Name Rel
  , rules :: Map Name Rule
  , trace :: String -> m Unit
  }

type Env =
  { props :: List Prop
  }

data Error = Error String

trace :: forall m. MonadEffect m => String -> M m Unit
trace msg = do
  ctx <- ask
  ctx.trace msg # lift # lift # lift

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. MonadEffect m => M m Unit
loop = do
  ctx <- ask
  env <- get
  new_props :: List Prop <- map fold do
    ctx.rules # (Map.toUnfoldable :: _ -> List _) # traverse \(rule_name /\ Rule rule) -> do
      applyRule env.props (Rule rule)
  new /\ props' :: Boolean /\ List Prop <- new_props # flip foldM (true /\ none) \(new /\ props') p -> do
    new' /\ props'' <- insertProp p props'
    pure $ (new || new') /\ props''
  modify_ _ { props = props' }
  loop # when new

applyRule :: forall m. MonadEffect m => List Prop -> Rule -> M m (List Prop)
applyRule props (Rule rule) = case rule.hyps of
  Nil -> pure $ singleton rule.prop
  Cons (PropHyp hyp) hyps ->
    map fold $ props # traverse \prop ->
      unify hyp prop >>= case _ of
        Nothing -> pure none
        Just sigma ->
          Rule rule { hyps = hyps }
            # subst_Rule
            # flip runReaderT { sigma }
            # (unwrap :: Identity _ -> _)
            # applyRule props
  Cons (CompHyp _x _c) hyps -> do
    -- TODO: run `c` and store result in x
    applyRule props $ Rule rule { hyps = hyps }
  Cons (CondHyp _a) hyps -> do
    -- TODO: evaluate `a`. if its truem, then continue
    -- TODO: if it has free vars, then its badly-scoped
    applyRule props $ Rule rule { hyps = hyps }

-- • if `p` is subsumed by something in `props`, then yields `props`
-- • if `p` subsumes some props `props'` in `props`, then yields `p : props - props'`
-- • otherwise, yields `p : props`
-- TODO: implement more efficiently
insertProp :: forall m. MonadEffect m => Prop -> List Prop -> m (Boolean /\ List Prop)
insertProp p = flip foldM (true /\ none) \(new /\ props) q ->
  if p `subsumes` q then
    pure $ new /\ props
  else if q `subsumes` p then
    pure $ false /\ q : props
  else
    pure $ new /\ q : props

--------------------------------------------------------------------------------
-- unify
--------------------------------------------------------------------------------

-- unify p q = Just σ  <==>  σ p = q
unify :: forall m. MonadEffect m => Prop -> Prop -> M m (Maybe Subst)
unify (Prop _ p) (Prop _ q) = unify_Term p q

unify_Term :: forall m. MonadEffect m => Term -> Term -> M m (Maybe Subst)
unify_Term UnitTerm UnitTerm = pure $ pure Map.empty
unify_Term UnitTerm _ = pure none
unify_Term (NatTerm m) (NatTerm n) | m == n = pure $ pure Map.empty
unify_Term (NatTerm _) _ = pure none
unify_Term (VarTerm x) q = pure $ pure $ Map.singleton x q

--------------------------------------------------------------------------------
-- Subst
--------------------------------------------------------------------------------

type Subst = Map Name Term

type SubstM m = ReaderT { sigma :: Subst } m :: Type -> Type

subst_Rule :: forall m. MonadEffect m => Rule -> SubstM m Rule
subst_Rule (Rule rule) = do
  hyps <- rule.hyps # traverse subst_Hyp
  prop <- rule.prop # subst_Prop
  pure $ Rule { hyps, prop }

subst_Hyp :: forall m. MonadEffect m => Hyp -> SubstM m Hyp
subst_Hyp (PropHyp prop) = PropHyp <$> (prop # subst_Prop)
subst_Hyp (CompHyp x comp) = CompHyp x <$> (comp # subst_Comp)
subst_Hyp (CondHyp a) = CondHyp <$> (a # subst_Term)

subst_Prop :: forall m. MonadEffect m => Prop -> SubstM m Prop
subst_Prop (Prop r a) = Prop r <$> subst_Term a

subst_Comp :: forall m. MonadEffect m => Comp -> SubstM m Comp
subst_Comp (Invoke x args) = Invoke x <$> (args # traverse subst_Term)

subst_Term :: forall m. MonadEffect m => Term -> SubstM m Term
subst_Term UnitTerm = pure $ UnitTerm
subst_Term (NatTerm n) = pure $ NatTerm n
subst_Term a@(VarTerm x) = do
  { sigma } <- ask
  pure $ sigma # Map.lookup x # fromMaybe a

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

type SubsumesM m = ReaderT {} m

subsumes :: forall m. MonadEffect m => Prop -> Prop -> SubsumesM m Boolean
subsumes (Prop r1 p1) (Prop r2 p2) = todo "subsumes"

