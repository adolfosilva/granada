{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Granada.Ast where

-- |Given
{--
nonExistantActions :: Expr.Program -> [Expr.Action]
nonExistantActions program = map nonExistantActions characters
   where characters = charactersInProgram program
         as = actionsInProgram program

nonExistantActions' :: Expr.Character -> [Expr.Action] -> (Expr.Character, [Expr.Action])
nonExistantActions' character acts = (character, S.toList $ as S.\\ S.fromList acts)
   where as = S.fromList $ Expr.actions character
--}

{--
type Warning = String
type Error = String

unusedDefinition :: Warning
unusedDefinition = undefined

warnings :: [Warning]
warnings = [unusedDefinition]

errors :: [Error]
errors = [nonExistantActions]

data S = Err Error | Wrn Warning deriving (Show, Eq)
--}
