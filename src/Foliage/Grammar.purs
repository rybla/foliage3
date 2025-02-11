module Foliage.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)

data Prog = Prog (List Stmt)

derive instance Generic Prog _

instance Show Prog where
  show x = genericShow x

instance Eq Prog where
  eq x y = genericEq x y

data Stmt
  = DefRel Name Lat
  | DefRule Name Rule

derive instance Generic Stmt _

instance Show Stmt where
  show x = genericShow x

instance Eq Stmt where
  eq x y = genericEq x y

data Name = Name String

derive instance Generic Name _

fromNameToString :: Name -> String
fromNameToString (Name x) = x

instance Show Name where
  show x = genericShow x

instance Eq Name where
  eq x y = genericEq x y

instance Ord Name where
  compare x y = genericCompare x y

data Comp =
  Invoke Name (Array Term)

derive instance Generic Comp _

instance Show Comp where
  show x = genericShow x

instance Eq Comp where
  eq x y = genericEq x y

instance Ord Comp where
  compare x y = genericCompare x y

data Term
  = DataTerm DataTerm
  | VarTerm Name

derive instance Generic Term _

instance Show Term where
  show x = genericShow x

instance Eq Term where
  eq x y = genericEq x y

instance Ord Term where
  compare x y = genericCompare x y

data DataTerm
  = UnitTerm
  | BoolTerm Boolean
  | NatTerm Int

derive instance Generic DataTerm _

instance Show DataTerm where
  show x = genericShow x

instance Eq DataTerm where
  eq x y = genericEq x y

instance Ord DataTerm where
  compare x y = genericCompare x y

data Typ
  = UnitTyp
  | BoolType
  | NatType

derive instance Generic Typ _

instance Show Typ where
  show x = genericShow x

instance Eq Typ where
  eq x y = genericEq x y

instance Ord Typ where
  compare x y = genericCompare x y

data Lat
  = UnitLat
  | BoolLat
  | NatLat
  | DiscreteLat Lat -- everything is either equal or incomparable
  | OppositeLat Lat -- turn lattice upside-down

derive instance Generic Lat _

instance Show Lat where
  show x = genericShow x

instance Eq Lat where
  eq x y = genericEq x y

instance Ord Lat where
  compare x y = genericCompare x y

data Rel = Rel Name

derive instance Generic Rel _

instance Show Rel where
  show x = genericShow x

instance Eq Rel where
  eq x y = genericEq x y

instance Ord Rel where
  compare x y = genericCompare x y

data Prop = Prop Rel Term

derive instance Generic Prop _

instance Show Prop where
  show x = genericShow x

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

instance Eq Hyp where
  eq x y = genericEq x y

instance Ord Hyp where
  compare x y = genericCompare x y

data Rule = Rule
  { hyps :: List Hyp
  , prop :: Prop
  }

derive instance Generic Rule _

instance Show Rule where
  show x = genericShow x

instance Eq Rule where
  eq x y = genericEq x y

instance Ord Rule where
  compare x y = genericCompare x y

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

names_Term :: Term -> Set Name
names_Term (DataTerm (UnitTerm)) = mempty
names_Term (DataTerm ((BoolTerm _))) = mempty
names_Term (DataTerm ((NatTerm _))) = mempty
names_Term (VarTerm x) = Set.singleton x
