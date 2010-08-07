module Baysig.Syntax.TokParser where

import Text.Parsec
import Baysig.Syntax.Lexer

--parsers on tokens

type EParser a = Parsec [Tok] () a

prim :: (Tok -> Maybe a) -> EParser a
prim mf = tokenPrim (\t -> show t)
                    (\pos t s -> incSourceColumn pos 1)
                    (mf)

opTok :: String -> EParser String
opTok nm = prim $ \t-> case t of 
                               Op s | s == nm -> Just s
                                    | otherwise -> Nothing
                               _ -> Nothing


bracketed par p = do satisfyT (==Open par)
                     e <- p
                     satisfyT (==Close par)
                     return e

tok :: Tok -> EParser Tok
tok t = prim $ \t2-> if t== t2 then Just t else Nothing


con_int :: EParser Int
con_int = prim $ \t-> case t of 
                           TokInt s -> Just s
                           _ -> Nothing

con_float :: EParser Double
con_float = prim $ \t-> case t of 
                           TokFloat s -> Just s
                           _ -> Nothing

identifierOrOp :: EParser String
identifierOrOp = prim $ \t-> case t of 
                           Id s -> Just s
                           Op s -> Just s
                           _ -> Nothing

identifier :: EParser String
identifier = prim $ \t-> case t of 
                           Id s -> Just s
                           _ -> Nothing


satisfyT :: (Tok -> Bool) -> EParser Tok
satisfyT p = prim $ \t-> if p t then Just t else Nothing


