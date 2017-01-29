module Granada.Ast where

import qualified Granada.Expr as Expr

import qualified Data.Set as Set

{--
programToAst :: Expr.Program -> Ast
programToAst = undefined
--}

isCharacter :: Expr.Item -> Bool
isCharacter (Expr.Chr _) = True
isCharacter _       = False

isAction :: Expr.Item -> Bool
isAction (Expr.Act _) = True
isAction _       = False

isGoal :: Expr.Item -> Bool
isGoal (Expr.Gl _) = True
isGoal _      = False

itemToCharacter :: Expr.Item -> Expr.Character
itemToCharacter (Expr.Chr character) = character
itemToCharacter _ = error "Expected a Character"

itemToAction :: Expr.Item -> Expr.Action
itemToAction (Expr.Act action) = action
itemToAction _ = error "Expected an Action"

itemToGoal :: Expr.Item -> Expr.Goal
itemToGoal (Expr.Gl goal) = goal
itemToGoal _ = error "Expected a Goal"

-- |Returns a list of all the Expr.Character declared in a Expr.Program.
-- TODO: use fold?
charactersInProgram :: Expr.Program -> [Expr.Character]
charactersInProgram = map itemToCharacter . filter isCharacter

-- |Returns a list of all the Expr.Action declared in a Expr.Program.
actionsInProgram :: Expr.Program -> [Expr.Action]
actionsInProgram = map itemToAction . filter isAction

-- |Returns a list of all the Expr.Goal declared in a Expr.Program.
goalsInProgram :: Expr.Program -> [Expr.Goal]
goalsInProgram = map itemToGoal . filter isGoal

-- |Given
{--
nonExistantActions :: Expr.Program -> [Expr.Action]
nonExistantActions program = map nonExistantActions characters
   where characters = charactersInProgram program
         as = actionsInProgram program

nonExistantActions' :: Expr.Character -> [Expr.Action] -> (Expr.Character, [Expr.Action])
nonExistantActions' character acts = (character, Set.toList $ as Set.\\ Set.fromList acts)
   where as = Set.fromList $ Expr.actions character
--}

{--
warnings :: [Warning]
warnings = undefined

errors :: [Error]
errors = [nonExistantActions]
--}
