module Foliage.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data Name = Name String

derive instance Generic Name _

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
  = UnitTerm
  | NatTerm Int
  | VarTerm Name

derive instance Generic Term _

instance Show Term where
  show x = genericShow x

instance Eq Term where
  eq x y = genericEq x y

instance Ord Term where
  compare x y = genericCompare x y

data Typ = UnitTyp

derive instance Generic Typ _

instance Show Typ where
  show x = genericShow x

instance Eq Typ where
  eq x y = genericEq x y

instance Ord Typ where
  compare x y = genericCompare x y

data Lat
  = UnitLat
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

