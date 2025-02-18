module Foliage.Engine where

import Foliage.Grammar (Comp(..), DataTerm(..), Fun, Hyp(..), Lat, Name, Prog, Prop(..), Rel(..), Rule(..), Term(..), defs_of_Prog, vars_Term)
import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (fold, sequence, traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Unfoldable1 (singleton)
import Effect.Aff (Milliseconds)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Foliage.Pretty (parens, pretty)
import Foliage.Ui.Common (Error)
import Foliage.Utility (bug, todo)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M m = ReaderT (Ctx m) (StateT Env (ExceptT Error m))

type Ctx m =
  { relLats :: Map Name Lat
  , rules :: Map Name Rule
  , funs :: Map Name Fun
  , set_props :: List Prop -> m Unit
  , trace :: String -> PlainHTML -> m Unit
  , delay_duration :: Milliseconds
  , stopped :: m Boolean
  }

type Env =
  { gas :: Int
  , props :: List Prop
  }

set_props :: forall m. MonadAff m => List Prop -> M m Unit
set_props props = do
  modify_ _ { props = props }
  ctx <- ask
  ctx.set_props props # lift # lift # lift
  pure unit

trace :: forall m. MonadAff m => String -> PlainHTML -> M m Unit
trace tag msg = do
  ctx <- ask
  ctx.trace tag msg # lift # lift # lift

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main
  :: forall m
   . MonadAff m
  => { prog :: Prog
     , initial_gas :: Int
     , delay_duration :: Milliseconds
     , set_props :: List Prop -> m Unit
     , trace :: String -> PlainHTML -> m Unit
     , stopped :: m Boolean
     }
  -> ExceptT Error m
       { props :: List Prop }
main args = do
  let { relLats, rules, funs } = defs_of_Prog args.prog
  { props } <-
    ( do
        trace "main" $ HH.text "begin"
        loop
        trace "main" $ HH.text "end"
    )
      # flip runReaderT
          { relLats
          , rules
          , funs: funs
          , delay_duration: args.delay_duration
          , set_props: args.set_props
          , trace: args.trace
          , stopped: args.stopped
          }
      # flip execStateT
          { gas: args.initial_gas
          , props: none
          }
  pure { props }

--------------------------------------------------------------------------------
-- loop
--------------------------------------------------------------------------------

loop :: forall m. MonadAff m => M m Unit
loop = do
  ctx <- ask
  env <- get
  trace "loop" $ HH.text $ "loop where gas = " <> pretty env.gas
  when (env.gas <= 0) do
    throwError [ HH.div [] [ HH.text "out of gas" ] ]
  new_props :: List Prop <- map fold do
    ctx.rules # (Map.toUnfoldable :: _ -> List _) # traverse \(name /\ Rule rule) -> do
      Rule rule # applyRule name 0 # flip runReaderT env.props
  trace "loop" $ HH.text $ "new_props = " <> pretty new_props
  new /\ props' :: Boolean /\ List Prop <-
    new_props # flip foldM (false /\ env.props) \(new /\ props') p -> do
      new' /\ props'' <- insertProp p props'
      trace "loop" $ HH.text $ "insertProp " <> parens (pretty p) <> " " <> parens (pretty props') <> " -> " <> parens (pretty (new' /\ props''))
      pure $ (new || new') /\ props''
  trace "loop" $ HH.text $ "new = " <> pretty new
  trace "loop" $ HH.text $ "props' = " <> pretty props'
  set_props props'
  modify_ _ { gas = env.gas - 1 }
  Aff.delay ctx.delay_duration # liftAff
  whenM (ctx.stopped # lift # lift # lift) do
    throwError [ HH.text "stopped" ]
  when new loop

applyRule :: forall m. MonadAff m => Name -> Int -> Rule -> ReaderT (List Prop) (M m) (List Prop)
applyRule name hyps_satisfied (Rule rule) = do
  -- lift $ trace "applyRule" $ HH.text $ "try " <> pretty name <> " hypothesis #" <> show hyps_satisfied
  case rule.hyps of
    Nil -> do
      lift $ trace "applyRule" $ HH.text $ pretty name <> "  ==>  " <> pretty rule.prop
      pure $ singleton rule.prop
    Cons (PropHyp hyp) hyps -> do
      props <- ask
      -- for each known `prop`
      map fold $ props # traverse \prop -> do
        -- check if prop subsumes hyp
        prop `subsumes` hyp # runExceptT # lift >>= case _ of
          Left _ -> do
            lift $ trace "applyRule" $ HH.text $ pretty name <> "#" <> show hyps_satisfied <> " ✗ " <> pretty hyp <> " by " <> pretty prop
            pure none
          -- if it can, then update rest of the rule with resulting `sigma`,
          -- then apply the rest of the rule
          Right sigma -> do
            prop' <- prop # subst_Prop # flip runReaderT { sigma } # lift
            lift $ trace "applyRule" $ HH.text $ pretty name <> "#" <> show hyps_satisfied <> " ✓ " <> pretty hyp <> " by " <> pretty prop'
            rule' <- Rule rule { hyps = hyps } # subst_Rule # flip runReaderT { sigma } # lift
            rule' # applyRule name (hyps_satisfied + 1)
    Cons (CompHyp x c) hyps -> do
      a <- runComp c # lift
      rule' <- Rule rule { hyps = hyps } # subst_Rule # flip runReaderT { sigma: Map.singleton x a } # lift
      rule' # applyRule name (hyps_satisfied + 1)
    Cons (CondHyp _a) hyps -> do
      -- TODO: evaluate `a`. if its true, then continue
      -- TODO: if it has free vars, then its badly-scoped
      Rule rule { hyps = hyps } # applyRule name (hyps_satisfied + 1)

runComp :: forall m. MonadAff m => Comp -> M m Term
runComp (Invoke x args) = do
  ctx <- ask
  f <- ctx.funs # Map.lookup x # flip maybe pure
    (throwError [ HH.text "runComp", HH.text $ "unknown fun name: " <> pretty x ])
  f args # flip either pure
    (\err -> throwError [ HH.text "runComp", HH.text $ "fun: " <> pretty x, HH.text $ "err = " <> err ])

-- • if `p` is subsumed by something in `props`, then yields `props`
-- • if `p` subsumes some props `props'` in `props`, then yields `p : props - props'`
-- • otherwise, yields `p : props`
-- TODO: implement more efficiently
insertProp :: forall m. MonadAff m => Prop -> List Prop -> M m (Boolean /\ List Prop)
insertProp p props = go # map \(new /\ props') -> new /\ (if new then p : props' else props')
  where
  go = props # flip foldM (true /\ none) \(new /\ props') q -> do
    p `latCompare` q # runExceptT >>= case _ of
      Left _ -> pure $ new /\ (q : props')
      Right (_ /\ LT) -> pure $ false /\ (q : props')
      Right (_ /\ EQ) -> pure $ false /\ (q : props')
      Right (_ /\ GT) -> pure $ new /\ props'

--------------------------------------------------------------------------------
-- subsumes
--------------------------------------------------------------------------------

subsumes :: forall m. MonadAff m => Prop -> Prop -> ExceptT String (M m) Subst
subsumes p q = do
  sigma /\ o <- latCompare p q
  case o of
    GT -> pure sigma
    EQ -> pure sigma
    _ -> throwError $ pretty p <> " is subsumed by " <> pretty q

subsumes_Term :: forall m. MonadAff m => Lat -> Term -> Term -> ExceptT String (M m) Subst
subsumes_Term l a b = do
  sigma /\ o <- latCompare_Term l a b
  case o of
    GT -> pure sigma
    EQ -> pure sigma
    _ -> throwError $ "In lattice " <> pretty l <> ", " <> pretty a <> " is subsumed by " <> pretty b

--------------------------------------------------------------------------------
-- latCompare
--------------------------------------------------------------------------------

latCompare :: forall m. MonadAff m => Prop -> Prop -> ExceptT String (M m) (Subst /\ Ordering)
latCompare (Prop r1 a1) (Prop r2 a2) | r1 == r2 = do
  l <- fromRelGetLat r1 # lift
  latCompare_Term l a1 a2
latCompare p q = throwError $ pretty p <> " is not comparable to " <> pretty q

latCompare_Term :: forall m. MonadAff m => Lat -> Term -> Term -> ExceptT String (M m) (Subst /\ Ordering)
latCompare_Term l a b = do
  o /\ sys_ <- latCompare_Term' l a b # runWriterT
  let
    sys =
      sys_
        # map (rmap (rmap Array.singleton))
        # Map.fromFoldableWith \(l1 /\ ts1) (l2 /\ ts2) ->
            if l1 /= l2 then bug $ "what do i do when two uses of a variable use the variable in a different lattice?"
            else l1 /\ (ts1 <> ts2)
  sigma <- solveSys sys
  pure $ sigma /\ o

latCompare_Term' :: forall m. MonadAff m => Lat -> Term -> Term -> WriterT (Array (Name /\ Lat /\ Term)) (ExceptT String (M m)) Ordering
-- VarTerm
latCompare_Term' l (VarTerm x1) (VarTerm x2) = do
  tell [ x1 /\ l /\ VarTerm x2 ]
  pure EQ
latCompare_Term' l (VarTerm x1) a2 = do
  tell [ x1 /\ l /\ a2 ]
  pure GT
latCompare_Term' l a1 (VarTerm x2) = do
  tell [ x2 /\ l /\ a1 ]
  pure LT
-- TODO
latCompare_Term' _ _ _ = empty

--------------------------------------------------------------------------------
-- Sys
--------------------------------------------------------------------------------

type Sys = Map Name (Lat /\ Array Term)

type Sys1 = Map Name (Lat /\ Term)

-- Solves the system for a consistent substitution
solveSys :: forall m. MonadAff m => Sys -> ExceptT String (M m) Subst
solveSys sys = do
  -- unify multiple assignments of the same var until only single assignment per var
  let
    go :: Sys -> ExceptT String (M m) Sys1
    go sys = do
      -- consistency (occurs) check
      sys # traverseWithIndex_ \x (_ /\ ts) -> do
        when (x `Set.member` (ts # map vars_Term # Set.unions)) do
          throwError $ "variable " <> pretty x <> " was assigned to the terms " <> pretty ts <> " which include a reference to the variable itself"
      sys :: List (Map Name (ExceptT String (M m) (Lat /\ Array Term))) <- sys # (Map.toUnfoldable :: _ -> List _) # traverse \(x /\ l /\ ts) -> do
        case ts of
          [ t ] -> pure $ Map.singleton x (pure (l /\ [ t ]))
          _ -> do
            sys /\ t <- unifyTerms l ts
            pure $ Map.insert x (pure (l /\ [ t ])) (sys # map pure)
      sys :: Sys <-
        foldM
          ( \sys msys' ->
              Map.unionWith
                ( \mlt1 mlt2 -> do
                    l1 /\ ts1 <- mlt1
                    l2 /\ ts2 <- mlt2
                    when (l1 /= l2) do throwError "if a variable is assigned under two different lattice orderings, then we can't compare"
                    pure (l1 /\ (ts1 <> ts2))
                )
                (sys # map pure)
                msys' # sequence
          )
          Map.empty
          sys
      case
        sys # traverse \(l /\ ts) -> case ts of
          [ t ] -> pure (l /\ t)
          _ -> none
        of
        Nothing -> go sys
        Just sys1 -> pure sys1
  sys1 <- go sys
  -- apply substitution to each assignment body
  sys1 :: Sys1 <- sys1 # traverse \(l /\ t) -> (l /\ _) <$> (t # subst_Term >>> flip runReaderT { sigma: sys1 # map snd } >>> lift)
  pure $ sys1 # map snd

unifyTerms :: forall m. MonadAff m => Lat -> Array Term -> ExceptT String (M m) (Sys /\ Term)
unifyTerms = todo ""

--------------------------------------------------------------------------------
-- old
--------------------------------------------------------------------------------

-- latCompare (Prop r1 a1) (Prop r2 a2) = do
--   if r1 /= r2 then
--     pure $ empty /\ Nothing
--   else do
--     l <- fromRelGetLat r1
--     latCompare_Term l a1 a2

-- type LatCompareM m = WriterT Sys (M m)

-- latCompare_Term :: forall m. MonadAff m => Lat -> Term -> Term -> M m (Subst /\ LatOrdering)
-- latCompare_Term l a b = do
--   -- sys <- latCompare_Term' l a b # runWriterT
--   --       solveSysForSubst
--   --   # runMaybeT
--   --   >>= case _ of
--   --     Nothing -> pure $ empty /\ Nothing
--   --     Just sigma -> pure $ sigma /\ lo
--   lo /\ sys <- latCompare_Term' l a b # runWriterT
--   sys # solveSysForSubst # runMaybeT >>= case _ of
--     Nothing -> todo ""
--     Just _ -> todo ""

-- latCompare_Term' :: forall m. MonadAff m => Lat -> Term -> Term -> LatCompareM m LatOrdering

-- latCompare_Term' l (VarTerm x1) (VarTerm x2) = do
--   tell $ pure $ x1 /\ l /\ VarTerm x2
--   pure $ Just EQ
-- latCompare_Term' l (VarTerm x1) a2 = do
--   tell $ pure $ x1 /\ l /\ a2
--   pure $ Just GT
-- latCompare_Term' l a1 (VarTerm x2) = do
--   tell $ pure $ x2 /\ l /\ a1
--   pure $ Just LT

-- latCompare_Term' UnitLat _ _ =
--   pure $ Just EQ

-- latCompare_Term' IntLat (DataTerm (IntTerm n1)) (DataTerm (IntTerm n2)) =
--   pure $ Just $ compare n1 n2

-- latCompare_Term' (ProdLat LexicographicProdLatOrdering a b) (PairTerm x1 y1) (PairTerm x2 y2) =
--   latCompare_Term' a x1 x2 >>= case _ of
--     Just LT -> pure $ Just LT
--     Just EQ -> latCompare_Term' b y1 y2
--     Just GT -> pure $ Just GT
--     Nothing -> pure Nothing

-- latCompare_Term' (DiscreteLat l) a1 a2 =
--   latCompare_Term' l a1 a2 >>= case _ of
--     Just EQ -> pure $ Just EQ
--     _ -> pure Nothing

-- latCompare_Term' (OppositeLat l) a1 a2 =
--   latCompare_Term' l a1 a2 >>= case _ of
--     Nothing -> pure Nothing
--     Just o -> pure $ Just $ invert o

-- latCompare_Term' l a1 a2 = throwError [ HH.text "latCompare_Term'", HH.text $ "terms are not compatible with lattice " <> pretty l <> ": " <> pretty a1 <> ", " <> pretty a2 ]

--------------------------------------------------------------------------------
-- Subst
--------------------------------------------------------------------------------

type Subst = Map Name Term

type SubstM m = ReaderT { sigma :: Subst } (M m) :: Type -> Type

subst_Rule :: forall m. MonadAff m => Rule -> SubstM m Rule
subst_Rule (Rule rule) = do
  hyps <- rule.hyps # traverse subst_Hyp
  prop <- rule.prop # subst_Prop
  pure $ Rule { hyps, prop }

subst_Hyp :: forall m. MonadAff m => Hyp -> SubstM m Hyp
subst_Hyp (PropHyp prop) = PropHyp <$> (prop # subst_Prop)
subst_Hyp (CompHyp x comp) = CompHyp x <$> (comp # subst_Comp)
subst_Hyp (CondHyp a) = CondHyp <$> (a # subst_Term)

subst_Prop :: forall m. MonadAff m => Prop -> SubstM m Prop
subst_Prop (Prop r a) = Prop r <$> subst_Term a

subst_Comp :: forall m. MonadAff m => Comp -> SubstM m Comp
subst_Comp (Invoke x args) = Invoke x <$> (args # traverse subst_Term)

subst_Term :: forall m. MonadAff m => Term -> SubstM m Term
subst_Term a@(VarTerm x) = do
  { sigma } <- ask
  pure $ sigma # Map.lookup x # fromMaybe a
subst_Term (DataTerm UnitTerm) = pure $ DataTerm UnitTerm
subst_Term (DataTerm (BoolTerm b)) = pure $ DataTerm (BoolTerm b)
subst_Term (DataTerm (IntTerm n)) = pure $ DataTerm (IntTerm n)
subst_Term (PairTerm a b) = PairTerm <$> subst_Term a <*> subst_Term b

-- --------------------------------------------------------------------------------
-- -- utilities
-- --------------------------------------------------------------------------------

-- type Sys = List (Name /\ Lat /\ Term)

-- -- solves system of assignments for a consistent substitution that subsumes them
-- solveSysForSubst :: forall m. MonadAff m => Sys -> MaybeT (M m) Subst
-- solveSysForSubst = init empty >=> go
--   where
--   init :: Map Name (Lat /\ Term) -> Sys -> _ Subst
--   init sigma Nil = pure (sigma # map snd)
--   init sigma ((x /\ l /\ a) : sys) = case sigma # Map.lookup x of
--     -- if assigning `x` to `a` doesn't conflict with any existing assignments, 
--     -- then just do it
--     Nothing -> init (sigma # Map.insert x (l /\ a)) sys
--     -- if assigning x to a conflicts with a previous assignment of x to a_,
--     -- then check a `subsumes` a_
--     -- then append any resulting substitution from that to the rest of the system to be initialized
--     Just (l_ /\ a_) -> do
--       if l /= l_ then
--         bug $ "is it an actual case to handle when vars in a system are ordered according to different lattices?"
--       else do 
--         sigma' <- subsumes_Term l a a_ 
--         a' <- a # subst_Term # flip runReaderT { sigma: sigma' } # lift
--         init (sigma # Map.insert x (l /\ a')) (sys <> Map.toUnfoldable sigma')

--         -- # lift >>= case _ of
--         --   Nothing -> empty
--         --   Just sys' -> do
--         --     let sigma' = Map.fromFoldable sys' -- TODO: handle solving system
--         --     a' <- a # subst_Term # flip runReaderT { sigma: sigma' } # lift
--         --     init (sigma # Map.insert x a') (sys <> Map.toUnfoldable sigma')

--   go :: Subst -> _ Subst
--   go sigma = do
--     sigma' <- sigma # traverseWithIndex \x a -> do
--       when (x `occursIn` a) do empty
--       a # subst_Term # flip runReaderT { sigma } # lift
--     if sigma /= sigma' then
--       go sigma'
--     else
--       pure sigma

--   occursIn x a = x `Set.member` names_in_Term a

fromRelGetLat :: forall m. MonadAff m => Rel -> M m Lat
fromRelGetLat (Rel x) = do
  { relLats } <- ask
  relLats
    # Map.lookup x
    # maybe (throwError [ HH.text "fromRelGetLat", HH.text $ "unknown relation name: " <> pretty x ]) pure

