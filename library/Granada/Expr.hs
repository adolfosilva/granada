module Granada.Expr where

import Data.Map (Map)

-- |A program is just a series of items.
type Program = [Item]

-- |An Item can be an Action, a Goal or a Character.
data Item = Act Action | Gl Goal | Chr Character deriving (Show, Read, Eq)

-- |A Name is just a 'String'.
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

-- | Checks if an 'Item' is a 'Character'.
isCharacter :: Item -> Bool
isCharacter (Chr _) = True
isCharacter _            = False

-- | Checks if an 'Item' is an 'Action'.
isAction :: Item -> Bool
isAction (Act _) = True
isAction _            = False

-- | Checks if an 'Item' is a 'Goal'.
isGoal :: Item -> Bool
isGoal (Gl _) = True
isGoal _           = False

-- | Unpacks a 'Character' from an 'Item' if possible.
itemToCharacter :: Item -> Character
itemToCharacter (Chr character) = character
itemToCharacter _                    = error "Expected a Character"

-- | Unpacks an 'Action' from an 'Item' if possible.
itemToAction :: Item -> Action
itemToAction (Act action) = action
itemToAction _                 = error "Expected an Action"

-- | Unpacks a 'Goal' from an 'Item' if possible.
itemToGoal :: Item -> Goal
itemToGoal (Gl goal) = goal
itemToGoal _              = error "Expected a Goal"

-- |Returns a list of all the Character declared in a Program.
-- TODO: use fold?
charactersInProgram :: Program -> [Character]
charactersInProgram = map itemToCharacter . filter isCharacter

-- |Returns a list of all the Action declared in a Program.
actionsInProgram :: Program -> [Action]
actionsInProgram = map itemToAction . filter isAction

-- |Returns a list of all the Goal declared in a Program.
goalsInProgram :: Program -> [Goal]
goalsInProgram = map itemToGoal . filter isGoal

