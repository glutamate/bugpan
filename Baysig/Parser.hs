module Baysig.Parser where

import Baysig.Lexer
import Baysig.Expr
import Data.List
import Text.Parsec.String 
import Text.Parsec.Expr
import Text.Parsec
import Data.Ord

instance Show Assoc where
    show (AssocLeft) = "AssocLeft"
    show (AssocRight) = "AssocRight"
                       

data FixDec = FixDec Assoc Int String
            deriving Show
stdFixity = [FixDec AssocLeft 2 "+",
             FixDec AssocLeft 3 "*"]

prec ( FixDec _ n _) = n

groupFixeties fds = groupBy (\f1 f2 -> prec f1 == prec f2) $ reverse $ sortBy (comparing prec) fds

type EParser a = Parsec [Tok] () a

parseE :: [FixDec] -> EParser E
parseE fds = expr
    where expr = try lam <|> buildExpressionParser table term <?> "expression"
          table =map (map mkOp) $ groupFixeties fds
          term =  try app <|> try (bracketed Parens expr)
                    <|> try var <|> eint  <?> "factor"
          app = do e1 <- try var <|> lam
                   e2 <- term
                   return $ EApp e1 e2
                <?> "application"
          var = EVar `fmap` identifier <?> "variable name"
          eint = (ECon . VInt) `fmap` con_int <?> "integer"
          lam = try (do 
                   opTok "\\" 
                   vnm <- identifier
                   opTok "->" 
                   e <- expr
                   return $ ELam (PVar vnm) e)
                <|> (bracketed Parens lam)
                <?> "lambda term"
mkOp (FixDec ass prn nm) = Infix (do (const (\e1 e2-> EVar nm $> e1 $> e2)) `fmap` opTok nm) ass

bracketed par p = do satisfyT (==Open par)
                     e <- p
                     satisfyT (==Close par)
                     return e

prim :: (Tok -> Maybe a) -> EParser a
prim mf = tokenPrim (\t -> show t)
                    (\pos t s -> incSourceColumn pos 1)
                    (mf)

con_int :: EParser Int
con_int = prim $ \t-> case t of 
                           TokInt s -> Just s
                           _ -> Nothing

con_float :: EParser Double
con_float = prim $ \t-> case t of 
                           TokFloat s -> Just s
                           _ -> Nothing


identifier :: EParser String
identifier = prim $ \t-> case t of 
                           Id s -> Just s
                           _ -> Nothing

opTok :: String -> EParser String
opTok nm = prim $ \t-> case t of 
                               Op s | s == nm -> Just s
                                    | otherwise -> Nothing
                               _ -> Nothing

satisfyT :: (Tok -> Bool) -> EParser Tok
satisfyT p = prim $ \t-> if p t then Just t else Nothing


parseFixDec :: Parser FixDec
parseFixDec = do string "infix"
                 lrlet <- letter
                 lr <- case lrlet of 
                         'l' -> return AssocLeft
                         'r' -> return AssocRight
                         _ -> fail $ "invalid fixity"
                 spaces
                 n <- number
                 spaces
                 op <- many1 (oneOf opChars)
                 return $ FixDec lr n op

parseDs :: String -> Either String [D]
parseDs in_s = 
    let lns = lines in_s
        isFixDec ('i':'n':'f':'i':'x':lr:_) = lr == 'r' || lr =='l'
        isFixDec ln = False
        (fixDecLns, program) = partition isFixDec lns
    in do fixDecs <- mapM (showErr . parse parseFixDec "") fixDecLns
          fail $ show $groupFixeties fixDecs

showErr :: Show e => Either e a -> Either String a
showErr (Left e) = Left $ show e
showErr (Right x) = Right x

number  :: Parser Int
number  = do ds <- many1 digit
             return (read ds)
        <?> "number"