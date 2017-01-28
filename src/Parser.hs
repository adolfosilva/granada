{-# LANGUAGE MultiParamTypeClasses #-}

module Parser (parseFromFile, program, action, goal, character) where

import           Control.Applicative
import           Data.Functor.Identity

import           System.Exit
import           System.IO             (hPutStrLn, stderr)
import qualified Text.Parsec           as Parsec
import qualified Text.Parsec.String    as SParsec

import           Data.Map              (Map)
import qualified Data.Map              as Map

import qualified Expr

-- Notes:
-- Both defined in Control.Applicative
-- (*>): Sequence actions, discarding the value of the first argument.
--       eg.: Just (+3) *> Just 9 results in Just 9
-- (<*): Sequence actions, discarding the value of the second argument.
--       eg.: Just 7 <* Just 8 results in Just 7
-- (<*>):  Unwrap the contents of both sides, combine them and wrap again.
--       eg.: Just (+7) <*> Just 7 results in Just 14
-- (<$>): Unwrap the contents of the right, combine the left and right arguments and return.
--        ex: (+3) <$> Just 9 results in Just 11
-- (<|>)

number :: Parsec.Parsec String () String
number = Parsec.many1 Parsec.digit

plus :: Parsec.Parsec String () String
plus = Parsec.char '+' *> number

minus :: Parsec.Parsec String () String
minus = (:) <$> Parsec.char '-' <*> number

int :: Parsec.Parsec String () String
int = plus <|> minus <|> number

-- |Parser for positive integers.
integer :: Parsec.Parsec String () Expr.Value
integer = Expr.VInt . read <$> int

-- |Parser for logical values "true" and "false".
boolean :: Parsec.Parsec String () Expr.Value
boolean = Expr.VBool . (== "true") <$> (Parsec.string "true" <|> Parsec.string "false")

-- |Parser for base types.
value :: Parsec.Parsec String () Expr.Value
value = boolean <|> integer

-- |Parser for CamelCase strings.
camelCaseString :: Parsec.Parsec String () String
camelCaseString = undefined
--camelCaseString = (:) upper <*> lower

identifier :: Parsec.Parsec String () String
identifier = var -- TODO: camelCaseString

-- |Parser for a comma ",".
comma :: Parsec.Parsec String () Char
comma = Parsec.char ','

-- |Parser for a colon ":".
colon :: Parsec.Parsec String () Char
colon = Parsec.char ':'

trimmed :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
trimmed p = Parsec.spaces *> p <* Parsec.spaces

braces :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
braces = Parsec.between (Parsec.string "{") (Parsec.string "}")

parens :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
parens = Parsec.between (Parsec.string "(") (Parsec.string ")")

brackets :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
brackets = Parsec.between (Parsec.string "[") (Parsec.string "]")

-- |Parser for a possibly empty list of something.
list :: Parsec.Parsec String () a -> Parsec.Parsec String () [a]
list p = brackets (p `Parsec.sepEndBy` trimmed comma)

-- |Parser for a non-empty list of something.
nonEmptyList :: Parsec.Parsec String () a -> Parsec.Parsec String () [a]
nonEmptyList p = brackets (p `Parsec.sepEndBy1` trimmed comma)

mapP :: Parsec.Parsec String () a -> Parsec.Parsec String () [a]
mapP p = braces (p `Parsec.sepEndBy` comma)

var :: Parsec.Parsec String () String
var = Parsec.many1 Parsec.letter

-- |Parser for a dictItem.
-- | dictItem := variable ":" value
dictItem :: Parsec.Parsec String () (String, Expr.Value)
dictItem = do
   key <- trimmed var
   _ <- colon
   val <- trimmed value
   return (key, val)

-- |Parses a Dict (key-value collection).
dict :: Parsec.Parsec String () (Map String Expr.Value)
dict = Map.fromList <$> mapP dictItem

preconditions :: Parsec.Parsec String () (Map String Expr.Value)
preconditions = Parsec.string "pre" *> trimmed colon *> dict

effects :: Parsec.Parsec String () (Map String Expr.Value)
effects = Parsec.string "post" *> trimmed colon *> dict

-- >parseTest Parser.action "action Shoot { pre : {hasWeapon:true}, post : {enemyDamage: 10}}"
-- Action {actionName = "Shoot", preconditions = Just (fromList [("hasWeapon",VBool True)]), effects = fromList [("enemyDamage",VInt 10)]}

action :: Parsec.Parsec String () Expr.Action
action = do
   _ <- Parsec.string "action"
   name <- trimmed identifier
   (pre, post) <- braces actionBody
   return $ Expr.Action name pre post

actionBody :: Parsec.Parsec String () (Maybe (Map String Expr.Value), Map String Expr.Value)
actionBody = do
   pre <- trimmed (Parsec.optionMaybe preconditions)
   _ <- comma
   post <- trimmed effects
   return (pre, post)

-- |Parser for a Goal.
-- TODO: wrong definition
goal :: Parsec.Parsec String () Expr.Goal
goal = do
   _ <- Parsec.string "goal"
   name <- trimmed identifier
   dic <- dict
   return $ Expr.Goal name dic

goals :: Parsec.ParsecT String () Identity [String]
goals = Parsec.string "goals" *> trimmed colon *> nonEmptyList identifier

actions :: Parsec.ParsecT String () Identity [String]
actions = Parsec.string "actions" *> trimmed colon *> list identifier

-- |Parser for a Character.
character :: Parsec.Parsec String () Expr.Character
character = do
   _ <- Parsec.string "character"
   name <- trimmed identifier
   (gs, as) <- braces characterBody
   return $ Expr.Character name gs as

characterBody :: Parsec.Parsec String () ([String],[String])
characterBody = do
   gs <- trimmed goals
   _ <- comma
   as <- trimmed actions
   return (gs, as)

-- |Parser for a Expr.Act (Action).
actionItem :: Parsec.Parsec String () Expr.Item
actionItem = Expr.Act <$> action

-- |Parser for a Expr.Gl (Goal).
goalItem :: Parsec.Parsec String () Expr.Item
goalItem = Expr.Gl <$> goal

-- |Parser for a Expr.Chr (Character).
characterItem :: Parsec.Parsec String () Expr.Item
characterItem = Expr.Chr <$> character

-- |Parser for an item (action, goal or character).
item :: Parsec.Parsec String () Expr.Item
item = Parsec.choice [actionItem, goalItem, characterItem]

-- |Parser for a program.
-- |A program is a list of items (actions, goals and characters)
program :: Parsec.Parsec String () Expr.Program
program = Parsec.many (trimmed item)

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Maybe a
parse p s = case Parsec.parse p "(source)" s of
   Left _  -> Nothing
   Right x -> Just x

parseFromFile :: SParsec.Parser a -> String -> IO a
parseFromFile p fileName = SParsec.parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

