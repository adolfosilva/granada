module Granada.Expr where

import Data.Map (Map)

-- |A program is just a series of items.
type Program = [Item]

-- |An Item can be an Action, a Goal or a Character.
data Item = Act Action | Gl Goal | Chr Character deriving (Show, Read, Eq)

type Name = String

-- |An Action constains a name, effects and optional preconditions.
data Action = Action {
   actionName    :: Name,
   preconditions :: Maybe Dict,
   effects       :: Dict
} deriving (Show, Read, Eq)

-- |A Goal is a the desired final state of a Character.
data Goal = Goal Name Dict deriving (Show, Read, Eq)

-- |A Character has a name, can perform some actions, and has one or more goals.
data Character = Character {
   characterName :: Name,
   actions       :: [Name],
   goals         :: [Name]
} deriving (Show, Read, Eq)

-- |A Dict is a map of strings to values.
type Dict = Map String Value

-- |A Value can either be a boolean or an integer.
data Value = VBool Bool | VInt Integer deriving (Show, Read, Eq)

