{-# LANGUAGE TypeOperators, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Baysig.Parser where

import Baysig.Lexer
import Baysig.Expr
import Baysig.Layout
import Data.List
import Text.Parsec.String 
import Text.Parsec.Expr
import Text.Parsec
import Data.Ord
import Data.Char
import Control.Monad.Identity
import Prelude hiding (lex)

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

type EParser a = Parsec [Tok] () a

--http://guppy.eng.kagawa-u.ac.jp/2005/AdvProg/Programs/IO.hs

reservedKW = words "let in case where"

parseE :: [FixDec] -> EParser E
parseE fds = expr
    where expr = buildExpressionParser table factor <?> "expression"
          table =map (map mkOp) $ groupFixeties fds
          factor =  foldl1 EApp `fmap` many1 term
          term =  try unitConstr <|>
                  bracketed Parens expr <|>
                  try var <|> 
                  eint <|>
                  edbl <|>
                  psig <|> 
                  psigval <|> 
                  try tyAnno <|>
                  plet <|> 
                  lam
                  <?> "factor"
          var = do nm <- identifier 
                   guard (nm `notElem` reservedKW)
                   return $ EVar nm
                <?> "variable name"
          eint = (ECon . VInt) `fmap` con_int <?> "integer"
          edbl = (ECon . VReal) `fmap` con_float <?> "floating point number"
          lam = do 
                   opTok "\\" 
                   vnm <- identifier
                   opTok "->" 
                   e <- expr
                   return $ ELam (PVar vnm) e
                <?> "lambda term"
          tyAnno = do ty <- parseTy
                      opTok ":"
                      e <- expr
                      return $ ETy ty e
                   <?> "type annotation"
          psig = EApp (EVar "sig") `fmap` bracketed Sig expr  <?> "signal expression"
          psigval = EApp (EVar "sigval") `fmap` bracketed SigVal expr  <?> "signal valueexpression"
          unitConstr = do bracketed Parens (return ())
                          return $ EConstruct "unit" []
          plet = do tok $ Id "let"
                    p <- parsePat
                    opTok "="
                    pe <- expr
                    tok $ Id "in"
                    re <- expr
                    return (ELet [(p, pe)] re)
                    <?> "let term"
                       

parsePat :: EParser Pat
parsePat = buildExpressionParser table pPat <?> "Pattern"
    where pPat = (bracketed Parens parsePat)
                 <|> (PVar `fmap` identifier)
                 <|> (const PWild `fmap` tok Underscore)
                 <|> ((PLit . VInt) `fmap` con_int)
                 <?> "pattern"
          table = [[bang_op]]
          bang_op = Postfix (const (\p-> PBang p) `fmap` opTok "!") 
parseTy :: EParser T
parseTy =  buildExpressionParser table pTy <?> "type expression"
    where pTy = foldl1 TApp `fmap` many1 term
          table = [[arr_op]]
          term = (bracketed Parens parseTy)
                 <|> atomic
          atomic = do vnm <- identifier
                      if isLower $ head vnm
                           then return $ TVar vnm
                           else return $ TCon vnm
          arr_op = Infix (const (\t1 t2-> TLam t1 t2) `fmap` opTok "->") AssocRight


parseD :: [FixDec] -> EParser D
parseD fds = try mktyd <|> try letd <|> try tyd  <|> try impd <|> try psink <|> psource
    where letd = do pats <- many1 parsePat
                    guard (PVar "data" /= head pats)
                    opTok "=" 
                    e <- parseE fds
                    return $ DLet pats e
          tyd = do ids <- sepBy1 identifier (opTok ",")
                   opTok "::"
                   ty <- parseTy
                   return $ DDecTy ids ty
          impd = do tok $ Id "import" 
                    nm <- identifier
                    return $ DImport nm
          mktyd = do tok (Id "data")
                     tynm <- identifier
                     tyvars <- many identifier
                     opTok "=" 
                     constrs <- sepBy parseC (opTok "|")
                     return $ (DMkType tynm tyvars constrs)
          psink = do e <- parseE fds
                     opTok "*>"
                     nm <- identifier
                     param <- parseE fds
                     return (DSink e nm param)
          psource = do p <- parsePat
                       opTok "<*"
                       nm <- identifier
                       param <- parseE fds
                       return (DSource p nm param)

          parseC :: EParser (String, [T])
          parseC = do cnm <- identifier
                      fields <- many parseTy
                      return (cnm, fields)
                            
            
            

mkOp (FixDec In ass _ nm) = Infix (const (\e1 e2-> EVar nm $> e1 $> e2) `fmap` opTok nm) ass
mkOp (FixDec Pre _ _ nm) = Prefix (const (\e1-> EVar nm $> e1 ) `fmap` opTok nm) 
mkOp (FixDec Post _ _ nm) = Postfix (const (\e1-> EVar nm $> e1 ) `fmap` opTok nm) 

bracketed par p = do satisfyT (==Open par)
                     e <- p
                     satisfyT (==Close par)
                     return e

tok :: Tok -> EParser Tok
tok t = prim $ \t2-> if t== t2 then Just t else Nothing

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
parseFixDec = try pin <|> try ppost <|> ppre <?> "fixity declaration"
    where pin = do string "infix"
                   lrlet <- letter
                   lr <- case lrlet of 
                           'l' -> return AssocLeft
                           'r' -> return AssocRight
                           '_' -> return AssocNone
                           _ -> fail $ "invalid fixity"
                   spaces
                   n <- number
                   spaces
                   op <- many1 (oneOf opChars)
                   return $ FixDec In lr n op
          ppre = do string "prefix"
                    spaces
                    n <- number
                    spaces
                    op <- many1 (oneOf opChars)
                    return $ FixDec Pre AssocNone n op
          ppost = do string "postfix"
                     spaces
                     n <- number
                     spaces
                     op <- many1 (oneOf opChars)
                     return $ FixDec Post AssocNone n op


parseDs :: String -> Either String [D]
parseDs in_s = 
    let lns = lines in_s
        isFixDec ('i':'n':'f':'i':'x':lr:_) = lr == 'r' || lr =='l'
        isFixDec ('p':'r':'e':'f':'i':'x':' ':_) = True
        isFixDec ('p':'o':'s':'t':'f':'i':'x':' ':_) = True
        isFixDec ln = False
        fixDecLns = filter isFixDec lns
        toks = map fst $ addDeclEnds $ lex 0 0 in_s
        
    in do fixDecs <- mapM (showErr . parse parseFixDec "") fixDecLns
          showErr $ parse (parseModNm >> (endBy (parseD fixDecs) (tok (EndOf Declaration)))) "" toks
          --fail $ show toks

parseModNm = do tok $ Id "module"
                nm <- identifier
                tok $ Id "where"
                tok $ EndOf Declaration
                <?> "module declaration"

showErr :: Show e => Either e a -> Either String a
showErr (Left e) = Left $ show e
showErr (Right x) = Right x

number  :: Parser Int
number  = do ds <- many1 digit
             return (read ds)
        <?> "number"