module Foliage.Grammar where

import Prelude

import Control.Alternative (empty)
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~))
import Data.Lens.At (at)
import Data.List (List, foldr)
import Data.Map (Map)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Foliage.Pretty (class Pretty, parens, pretty)
import Foliage.Utility (prop)

newtype Prog = Prog (List Stmt)

derive instance Generic Prog _

instance Show Prog where
  show x = genericShow x

instance Pretty Prog where
  pretty (Prog stmts) = stmts # map pretty # intercalate "\n"

data Stmt
  = DefRel Name Lat
  | DefRule Name Rule
  | DefFun Name (Array (Name /\ Typ)) Typ Fun

type Fun = Array Term -> Either String Term

derive instance Generic Stmt _

instance Show Stmt where
  show (DefRel x l) = "DefRel " <> show x <> " " <> show l
  show (DefRule x r) = "DefRule " <> show x <> " " <> show r
  show (DefFun x ps t _) = "DefFun " <> show x <> " " <> show ps <> " " <> show t <> " " <> "<fun>"

instance Pretty Stmt where
  pretty (DefRel x l) = "relation " <> pretty x <> " " <> parens (pretty l)
  pretty (DefRule x r) = "rule " <> pretty x <> " := " <> pretty r
  pretty (DefFun x ps t _) = "fun " <> pretty x <> parens (ps # map (\(x /\ t) -> pretty x <> " : " <> pretty t) # intercalate ", ") <> " -> " <> pretty t <> " := " <> "<fun>"

newtype Name = Name String

derive instance Generic Name _

instance Show Name where
  show x = genericShow x

instance Pretty Name where
  pretty (Name s) = s

instance Eq Name where
  eq x y = genericEq x y

instance Ord Name where
  compare x y = genericCompare x y

data Comp =
  Invoke Name (Array Term)

derive instance Generic Comp _

instance Show Comp where
  show x = genericShow x

instance Pretty Comp where
  pretty (Invoke x args) = pretty x <> " " <> parens (pretty args)

instance Eq Comp where
  eq x y = genericEq x y

instance Ord Comp where
  compare x y = genericCompare x y

data Term
  = VarTerm Name
  | DataTerm DataTerm
  | PairTerm Term Term

derive instance Generic Term _

instance Show Term where
  show x = genericShow x

instance Pretty Term where
  pretty (VarTerm x) = pretty x
  pretty (DataTerm a) = pretty a
  pretty (PairTerm a b) = parens (pretty a <> " , " <> pretty b)

instance Eq Term where
  eq x y = genericEq x y

instance Ord Term where
  compare x y = genericCompare x y

data DataTerm
  = UnitTerm
  | BoolTerm Boolean
  | IntTerm Int

derive instance Generic DataTerm _

instance Show DataTerm where
  show x = genericShow x

instance Pretty DataTerm where
  pretty UnitTerm = "unit"
  pretty (BoolTerm b) = show b
  pretty (IntTerm n) = show n

instance Eq DataTerm where
  eq x y = genericEq x y

instance Ord DataTerm where
  compare x y = genericCompare x y

data Typ
  = UnitTyp
  | BoolTyp
  | IntTyp
  | ProdTyp Typ Typ

derive instance Generic Typ _

instance Show Typ where
  show x = genericShow x

instance Pretty Typ where
  pretty UnitTyp = "⊤"
  pretty BoolTyp = "𝔹"
  pretty IntTyp = "ℤ"
  pretty (ProdTyp t u) = parens (pretty t <> " × " <> pretty u)

instance Eq Typ where
  eq x y = genericEq x y

instance Ord Typ where
  compare x y = genericCompare x y

data Lat
  = UnitLat
  | BoolLat
  | IntLat
  | DiscreteLat Lat -- everything is either equal or incomparable
  | OppositeLat Lat -- turn lattice upside-down

derive instance Generic Lat _

instance Show Lat where
  show x = genericShow x

instance Pretty Lat where
  pretty UnitLat = "𝕌"
  pretty BoolLat = "𝔹"
  pretty IntLat = "ℤ"
  pretty (DiscreteLat l) = "Discrete" <> parens (pretty l)
  pretty (OppositeLat l) = "Opposite" <> parens (pretty l)

instance Eq Lat where
  eq x y = genericEq x y

instance Ord Lat where
  compare x y = genericCompare x y

newtype Rel = Rel Name

derive instance Generic Rel _

instance Show Rel where
  show x = genericShow x

instance Pretty Rel where
  pretty (Rel x) = pretty x

instance Eq Rel where
  eq x y = genericEq x y

instance Ord Rel where
  compare x y = genericCompare x y

data Prop = Prop Rel Term

derive instance Generic Prop _

instance Show Prop where
  show x = genericShow x

instance Pretty Prop where
  pretty (Prop r a) = pretty r <> " " <> parens (pretty a)

instance Eq Prop where
  eq x y = genericEq x y

instance Ord Prop where
  compare x y = genericCompare x y

data Hyp
  = PropHyp Prop
  | CompHyp Name Comp
  | CondHyp Term

derive instance Generic Hyp _

instance Show Hyp where
  show x = genericShow x

instance Pretty Hyp where
  pretty (PropHyp p) = pretty p
  pretty (CompHyp x c) = pretty x <> " <- " <> pretty c
  pretty (CondHyp a) = "if " <> pretty a

instance Eq Hyp where
  eq x y = genericEq x y

instance Ord Hyp where
  compare x y = genericCompare x y

newtype Rule = Rule
  { hyps :: List Hyp
  , prop :: Prop
  }

derive instance Generic Rule _

instance Show Rule where
  show x = genericShow x

instance Pretty Rule where
  pretty (Rule rule) = pretty rule.hyps <> " ⊢ " <> pretty rule.prop

instance Eq Rule where
  eq x y = genericEq x y

instance Ord Rule where
  compare x y = genericCompare x y

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

names_in_Term :: Term -> Set Name
names_in_Term (VarTerm x) = Set.singleton x
names_in_Term (DataTerm (UnitTerm)) = mempty
names_in_Term (DataTerm ((BoolTerm _))) = mempty
names_in_Term (DataTerm ((IntTerm _))) = mempty
names_in_Term (PairTerm a b) = names_in_Term a <> names_in_Term b

defs_of_Prog
  :: Prog
  -> { relLats :: Map Name Lat
     , rules :: Map Name Rule
     , funs :: Map Name Fun
     }
defs_of_Prog (Prog stmts) =
  stmts # foldr go { relLats: empty, rules: empty, funs: empty }
  where
  go (DefRel x l) = prop @"relLats" <<< at x .~ pure l
  go (DefRule x r) = prop @"rules" <<< at x .~ pure r
  go (DefFun x _ _ f) = prop @"funs" <<< at x .~ pure f

