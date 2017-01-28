{-# LANGUAGE MultiParamTypeClasses #-}

module GameAI (GameAI, ActionContext) where

import Data.Dynamic
import Data.Map

type ActionContext = Map String Dynamic

data GameAI s a
    = Action (s -> s)
    | Idle
    | GameAI s a :*: GameAI s a
    | GameAI s a :|: GameAI s a
    | GameAI s a :>: GameAI s a
    | GameAI s a :||: GameAI s a
    | Fix (GameAI s a -> GameAI s a)
    | Atomic (GameAI s a)
    | Succeed a
    | Act (s -> ActionContext -> IO (GameAI s a))

class PerformAction s a where
    perform :: a -> s -> IO ()
    perform _ _ = undefined

many :: GameAI s a -> GameAI s a
many s = Fix (\x -> Action id :|: s :*: x)
