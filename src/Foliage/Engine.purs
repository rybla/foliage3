module Foliage.Engine where

import Foliage.Grammar
import Prelude

import Control.Alternative (empty)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ordering (invert)
import Data.Set as Set
import Data.Traversable (fold, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Unfoldable1 (singleton)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M m = (ReaderT (Ctx m) (StateT Env (ExceptT Error m)))

type Ctx m =
  { relLats :: Map Name Lat
  , rules :: Map Name Rule
  , trace :: PlainHTML -> m Unit
  }

type Env =
  { gas :: Int
  , props :: List Prop
  }

type Error = Array PlainHTML

trace :: forall m. Monad m => PlainHTML -> M m Unit
trace msg = do
  ctx <- ask
  ctx.trace msg # lift # lift # lift

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main
  :: forall m
   . Monad m
  => { prog :: Prog
     , gas :: Int
     , trace :: PlainHTML -> m Unit
     }
  -> ExceptT Error m
       { props :: List Prop }
main args = do
  let { relLats, rules } = defs_of_Prog args.prog
  { props } <-
    ( do
        trace $ HH.text "begin main"
        loop
        trace $ HH.text "end main"
    )
      # flip runReaderT
          { relLats
          , rules
          , trace: args.trace
          }
      # flip execStateT
          { gas: args.gas
          , props: none
          }
  pure { props }

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. Monad m => M m Unit
loop = do
  trace $ HH.text "begin loop"
  ctx <- ask
  env <- get
  when (env.gas <= 0) do
    throwError [ HH.text "out of gas" ]
  new_props :: List Prop <- map fold do
    ctx.rules # (Map.toUnfoldable :: _ -> List _) # traverse \(_rule_name /\ Rule rule) -> do
      Rule rule # applyRule # flip runReaderT env.props
  new /\ props' :: Boolean /\ List Prop <- new_props # flip foldM (true /\ none) \(new /\ props') p -> do
    new' /\ props'' <- insertProp p props'
    pure $ (new || new') /\ props''
  modify_ _
    { props = props'
    , gas = env.gas - 1
    }
  trace $ HH.text "begin loop"
  loop # when new

applyRule :: forall m. Monad m => Rule -> ReaderT (List Prop) (M m) (List Prop)
applyRule (Rule rule) = case rule.hyps of
  Nil -> pure $ singleton rule.prop
  Cons (PropHyp hyp) hyps -> do
    props <- ask
    -- for each known `prop`
    map fold $ props # traverse \prop ->
      -- check if `prop` can satisfy `hyp`
      unify hyp prop # runMaybeT # lift >>= case _ of
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
insertProp :: forall m. Monad m => Prop -> List Prop -> M m (Boolean /\ List Prop)
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
unify :: forall m. Monad m => Prop -> Prop -> MaybeT (M m) Subst
unify (Prop _ p) (Prop _ q) = unify_Term p q

unify_Term :: forall m. Monad m => Term -> Term -> MaybeT (M m) Subst
unify_Term (DataTerm a) (DataTerm b) | a == b = pure empty
unify_Term (VarTerm x) q = pure $ Map.singleton x q
unify_Term _ _ = empty

--------------------------------------------------------------------------------
-- Subst
--------------------------------------------------------------------------------

type Subst = Map Name Term

type SubstM m = ReaderT { sigma :: Subst } (M m) :: Type -> Type

subst_Rule :: forall m. Monad m => Rule -> SubstM m Rule
subst_Rule (Rule rule) = do
  hyps <- rule.hyps # traverse subst_Hyp
  prop <- rule.prop # subst_Prop
  pure $ Rule { hyps, prop }

subst_Hyp :: forall m. Monad m => Hyp -> SubstM m Hyp
subst_Hyp (PropHyp prop) = PropHyp <$> (prop # subst_Prop)
subst_Hyp (CompHyp x comp) = CompHyp x <$> (comp # subst_Comp)
subst_Hyp (CondHyp a) = CondHyp <$> (a # subst_Term)

subst_Prop :: forall m. Monad m => Prop -> SubstM m Prop
subst_Prop (Prop r a) = Prop r <$> subst_Term a

subst_Comp :: forall m. Monad m => Comp -> SubstM m Comp
subst_Comp (Invoke x args) = Invoke x <$> (args # traverse subst_Term)

subst_Term :: forall m. Monad m => Term -> SubstM m Term
subst_Term (DataTerm UnitTerm) = pure $ DataTerm UnitTerm
subst_Term (DataTerm (BoolTerm b)) = pure $ DataTerm (BoolTerm b)
subst_Term (DataTerm (NatTerm n)) = pure $ DataTerm (NatTerm n)
subst_Term a@(VarTerm x) = do
  { sigma } <- ask
  pure $ sigma # Map.lookup x # fromMaybe a

--------------------------------------------------------------------------------
-- latCompare
--------------------------------------------------------------------------------

type LatOrdering = Maybe Ordering

type LatCompareM m = M m

latCompare_Prop :: forall m. Monad m => Prop -> Prop -> LatCompareM m (Subst /\ LatOrdering)
latCompare_Prop (Prop r1 a1) (Prop r2 a2) = do
  if r1 == r2 then
    pure $ empty /\ Nothing
  else do
    l <- fromRelGetLat r1
    lo /\ sys <- latCompare_Term l a1 a2 # runWriterT
    solveSysForSubst sys # runMaybeT >>= case _ of
      Nothing -> pure $ empty /\ Nothing
      Just sigma -> pure $ sigma /\ lo

type LatCompareM' m = WriterT (Sys) (M m)

latCompare_Term :: forall m. Monad m => Lat -> Term -> Term -> LatCompareM' m LatOrdering

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

latCompare_Term NatLat (DataTerm (NatTerm n1)) (DataTerm (NatTerm n2)) =
  pure $ Just $ compare n1 n2

latCompare_Term (DiscreteLat l) a1 a2 =
  latCompare_Term l a1 a2 >>= case _ of
    Just EQ -> pure $ Just EQ
    _ -> pure Nothing

latCompare_Term (OppositeLat l) a1 a2 =
  latCompare_Term l a1 a2 >>= case _ of
    Nothing -> pure Nothing
    Just o -> pure $ Just $ invert o

latCompare_Term l a1 a2 = throwError [ HH.text "latCompare_Term", HH.text $ "terms are not compatible with lattice " <> show l <> ": " <> show a1 <> ", " <> show a2 ]

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

type Sys = List (Name /\ Term)

-- solves system of assignments for a consistent substitution that satisfies them
solveSysForSubst :: forall m. Monad m => Sys -> MaybeT (M m) Subst
solveSysForSubst = init empty >=> go
  where
  init :: Subst -> Sys -> _ Subst
  init sigma Nil = pure sigma
  init sigma ((x /\ a) : sys) = case sigma # Map.lookup x of
    -- if assigning `x` to `a` doesn't conflict with any existing assignments, 
    -- then just do it
    Nothing -> init (sigma # Map.insert x a) sys
    -- if assigning `x` to `a` conflicts with a previous assignment of `x` to `a'`,
    -- then unify `a` and `a'`,
    -- then append any resulting substitution from that to the rest of the system to be initialized
    Just a' -> do
      unify_Term a a' # runMaybeT # lift >>= case _ of
        Nothing -> empty
        Just sigma' -> do
          a'' <- a # subst_Term # flip runReaderT { sigma: sigma' } # lift
          init (sigma # Map.insert x a'') (sys <> Map.toUnfoldable sigma')

  go :: Subst -> _ Subst
  go sigma = do
    sigma' <- sigma # traverseWithIndex \x a -> do
      when (x `occursIn` a) do empty
      a # subst_Term # flip runReaderT { sigma } # lift
    if sigma /= sigma' then
      go sigma'
    else
      pure sigma

  occursIn x a = x `Set.member` name_of_Term a

fromRelGetLat :: forall m. Monad m => Rel -> M m Lat
fromRelGetLat (Rel x) = do
  { relLats } <- ask
  relLats
    # Map.lookup x
    # maybe (throwError [ HH.text "fromRelGetLat", HH.text $ "unknown relation name: " <> show x ]) pure

