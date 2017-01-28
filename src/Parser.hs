{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Control.Applicative
import Data.Functor.Identity

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as SParsec
import System.Exit
import System.IO (hPutStrLn, stderr)

import Data.Map (Map)
import qualified Data.Map as Map

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

braces :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
braces = Parsec.between (Parsec.string "{") (Parsec.string "}")

parens :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
parens = Parsec.between (Parsec.string "(") (Parsec.string ")")

brackets :: Parsec.Stream s m Char => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
brackets = Parsec.between (Parsec.string "[") (Parsec.string "]")

-- |Parser for a possibly empty list of something.
list :: Parsec.ParsecT String () Data.Functor.Identity.Identity a -> Parsec.ParsecT String () Data.Functor.Identity.Identity [a]
list p = brackets (p `Parsec.sepEndBy` comma)

-- |Parser for a non-empty list of something.
nonEmptyList :: Parsec.ParsecT String () Data.Functor.Identity.Identity a -> Parsec.ParsecT String () Data.Functor.Identity.Identity [a]
nonEmptyList p = brackets (p `Parsec.sepEndBy1` comma)

mapP :: Parsec.ParsecT String () Identity a -> Parsec.ParsecT String () Identity [a]
mapP p = braces (p `Parsec.sepEndBy` comma)

var :: Parsec.ParsecT String u Identity [Char]
var = Parsec.many1 Parsec.letter

-- |Parser for a dictItem.
-- | dictItem := variable ":" value
dictItem :: Parsec.Parsec String () (String, Expr.Value)
dictItem = do
   Parsec.spaces
   key <- var
   Parsec.spaces
   _ <- colon
   Parsec.spaces
   val <- value
   Parsec.spaces
   return (key, val) 

-- |Parses a Dict (key-value collection).
dict :: Parsec.Parsec String () (Map String Expr.Value)
dict = Map.fromList <$> mapP dictItem

preconditions :: Parsec.Parsec String () (Map String Expr.Value)
preconditions = Parsec.string "pre" *> Parsec.spaces *> colon *> Parsec.spaces *> dict

effects :: Parsec.Parsec String () (Map String Expr.Value)
effects = Parsec.string "post" *> Parsec.spaces *> colon *> Parsec.spaces *> dict

-- >parseTest Parser.action "action Shoot { pre : {hasWeapon:true}, post : {enemyDamage: 10}}"
-- Action {actionName = "Shoot", preconditions = Just (fromList [("hasWeapon",VBool True)]), effects = fromList [("enemyDamage",VInt 10)]}

action :: Parsec.Parsec String () Expr.Action
action = do
   _ <- Parsec.string "action"
   Parsec.spaces
   name <- identifier
   Parsec.spaces
   (pre, post) <- braces actionBody
   return $ Expr.Action name pre post
 
actionBody :: Parsec.ParsecT String () Identity (Maybe (Map String Expr.Value), Map String Expr.Value)
actionBody = do
   Parsec.spaces
   pre <- Parsec.optionMaybe preconditions
   Parsec.spaces
   _ <- comma
   Parsec.spaces
   post <- effects
   Parsec.spaces
   return (pre, post)

-- |Parser for a Goal.
-- TODO: wrong definition
goal :: Parsec.Parsec String () Expr.Goal
goal = do
   _ <- Parsec.string "goal"
   Parsec.spaces
   name <- identifier
   Parsec.spaces
   dic <- dict
   return $ Expr.Goal name dic

{--
actionItem :: Parsec.Parsec String () Expr.Item
actionItem = Expr.Act <*> action

goalItem :: Parsec.Parsec String () Expr.Item
goalItem = Expr.Gl <*> goal

-- |Parser for an item (action, goal or character).
item :: Parsec.Parsec String () Expr.Item
item = Parsec.choice [actionItem, goalItem] -- , character]
--}

{--
-- |Parser for a Character.
character :: Parsec.Parsec String () Expr.Character
character = undefined


-- |Parser for a program.
-- |A program is a list of items (actions, goals and characters)
program :: Parsec.Parsec String () Expr.Program
program = Parsec.spaces *> Parsec.many (item <* Parsec.spaces)
--}

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () a -> s -> Maybe a
parse p s = case (Parsec.parse p "(source)" s) of
   Left _ -> Nothing
   Right x -> Just x     

parseFromFile :: SParsec.Parser a -> String -> IO a
parseFromFile p fileName = SParsec.parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

