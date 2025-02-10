module Foliage.Engine where

import Foliage.Grammar
import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ordering (invert)
import Data.Traversable (fold, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Unfoldable1 (singleton)
import Effect.Class (class MonadEffect)
import Foliage.Utility (todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M m = (ReaderT (Ctx m) (StateT Env (ExceptT Error m)))

type Ctx m =
  { relLats :: Map Name Lat
  , rules :: Map Name Rule
  , trace :: String -> m Unit
  }

type Env =
  { props :: List Prop
  }

type Error = Array String

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
    ctx.rules # (Map.toUnfoldable :: _ -> List _) # traverse \(_rule_name /\ Rule rule) -> do
      Rule rule
        # applyRule
        # flip runReaderT env.props
  new /\ props' :: Boolean /\ List Prop <- new_props # flip foldM (true /\ none) \(new /\ props') p -> do
    new' /\ props'' <- insertProp p props'
    pure $ (new || new') /\ props''
  modify_ _ { props = props' }
  loop # when new

applyRule :: forall m. MonadEffect m => Rule -> ReaderT (List Prop) (M m) (List Prop)
applyRule (Rule rule) = case rule.hyps of
  Nil -> pure $ singleton rule.prop
  Cons (PropHyp hyp) hyps -> do
    props <- ask
    -- for each known `prop`
    map fold $ props # traverse \prop ->
      -- check if `prop` can satisfy `hyp`
      unify hyp prop # lift >>= case _ of
        Nothing -> pure none
        -- if it can, then update rest of the rule with resulting `sigma`,
        -- then apply the rest of the rule
        Just sigma -> do
          rule' <-
            Rule rule { hyps = hyps }
              # subst_Rule
              # flip runReaderT { sigma }
              # lift
          rule' # applyRule
  Cons (CompHyp _x _c) hyps -> do
    -- TODO: run `c` and store result in x
    Rule rule { hyps = hyps } # applyRule
  Cons (CondHyp _a) hyps -> do
    -- TODO: evaluate `a`. if its truem, then continue
    -- TODO: if it has free vars, then its badly-scoped
    Rule rule { hyps = hyps } # applyRule

-- • if `p` is subsumed by something in `props`, then yields `props`
-- • if `p` subsumes some props `props'` in `props`, then yields `p : props - props'`
-- • otherwise, yields `p : props`
-- TODO: implement more efficiently
insertProp :: forall m. MonadEffect m => Prop -> List Prop -> M m (Boolean /\ List Prop)
insertProp p = flip foldM (true /\ none) \(new /\ props) q -> do
  p `latCompare_Prop` q >>= case _ of
    _ /\ Nothing -> pure $ new /\ (q : props)
    _ /\ Just LT -> pure $ false /\ (q : props)
    _ /\ Just EQ -> pure $ false /\ props
    _ /\ Just GT -> pure $ new /\ props

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

type SubstM m = ReaderT { sigma :: Subst } (M m) :: Type -> Type

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
-- latCompare
--------------------------------------------------------------------------------

type LatOrdering = Maybe Ordering

type LatCompareM m = M m

latCompare_Prop :: forall m. MonadEffect m => Prop -> Prop -> LatCompareM m (Subst /\ LatOrdering)
latCompare_Prop (Prop r1 a1) (Prop r2 a2) = do
  if r1 == r2 then
    pure $ Map.empty /\ Nothing
  else do
    l <- fromRelGetLat r1
    lo /\ sys <-
      latCompare_Term l a1 a2
        # runWriterT
    sigma <- solveSystemForSubst sys
    pure $ sigma /\ lo

type LatCompareM' m = WriterT (List (Name /\ Term)) (M m)

latCompare_Term :: forall m. MonadEffect m => Lat -> Term -> Term -> LatCompareM' m LatOrdering

latCompare_Term _ (VarTerm x1) (VarTerm x2) = do
  tell $ pure $ x1 /\ VarTerm x2
  pure $ Just EQ
latCompare_Term _ (VarTerm x1) a2 = do
  tell $ pure $ x1 /\ a2
  pure $ Just GT
latCompare_Term _ a1 (VarTerm x2) = do
  tell $ pure $ x2 /\ a1
  pure $ Just LT

latCompare_Term UnitLat _ _ =
  pure $ Just EQ

latCompare_Term NatLat (NatTerm n1) (NatTerm n2) =
  pure $ Just $ compare n1 n2

latCompare_Term (DiscreteLat l) a1 a2 =
  latCompare_Term l a1 a2 >>= case _ of
    Just EQ -> pure $ Just EQ
    _ -> pure Nothing

latCompare_Term (OppositeLat l) a1 a2 =
  latCompare_Term l a1 a2 >>= case _ of
    Nothing -> pure Nothing
    Just o -> pure $ Just $ invert o

latCompare_Term l a1 a2 = throwError [ "latCompare_Term", "terms are not compatible with lattice " <> show l <> ": " <> show a1 <> ", " <> show a2 ]

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

-- solves system of assignments for a consistent substitution that satisfies them
solveSystemForSubst :: forall m. MonadEffect m => List (Name /\ Term) -> M m Subst
solveSystemForSubst = todo ""

fromRelGetLat :: forall m. MonadEffect m => Rel -> M m Lat
fromRelGetLat (Rel x) = do
  { relLats } <- ask
  relLats
    # Map.lookup x
    # maybe (throwError [ "fromRelGetLat", "unknown relation name: " <> show x ]) pure

