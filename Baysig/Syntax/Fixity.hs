module Baysig.Syntax.Fixity where

import Baysig.Syntax.Lexer
import Baysig.Syntax.TokParser
import Text.Parsec.Expr
import Data.List
import Data.Ord
import Baysig.Expr

instance Show Assoc where
    show AssocLeft  = "AssocLeft"
    show AssocNone  = "AssocNone"
    show AssocRight = "AssocRight"

data WhereFix = In | Post | Pre
            deriving Show

data FixDec = FixDec WhereFix Assoc Int String
            deriving Show

stdFixity = [FixDec In AssocLeft 2 "+",
             FixDec In AssocLeft 3 "*"]

prec ( FixDec _ _ n _) = n

groupFixeties fds = groupBy (\f1 f2 -> prec f1 == prec f2) $ reverse $ sortBy (comparing prec) fds

mkOpE (FixDec In ass _ nm) = Infix (const (\e1 e2-> EVar nm $> e1 $> e2) `fmap` opTok nm) ass
mkOpE (FixDec Pre _ _ nm) = Prefix (const (\e1-> EVar nm $> e1 ) `fmap` opTok nm) 
mkOpE (FixDec Post _ _ nm) = Postfix (const (\e1-> EVar nm $> e1 ) `fmap` opTok nm) 

mkOpP (FixDec In ass _ nm) = Infix (const (\e1 e2-> PCons nm [ e1, e2]) `fmap` opTok nm) ass
mkOpP (FixDec Pre _ _ nm) = Prefix (const (\e1-> PCons nm [e1] ) `fmap` opTok nm) 
mkOpP (FixDec Post _ _ nm) = Postfix (const (\e1-> PCons nm [e1] ) `fmap` opTok nm) 
