{-# LANGUAGE TypeOperators, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Baysig.Syntax.Parser where

import Baysig.Syntax.Lexer
import Baysig.Syntax.Fixity
import Baysig.Expr
import Baysig.Syntax.Layout
import Baysig.Syntax.TokParser
import Data.List
import Text.Parsec.String 
import Text.Parsec.Expr
import Text.Parsec
import Data.Ord
import Data.Char
import Control.Monad.Identity
import Prelude hiding (lex)
import Debug.Trace

reservedKW = words "let in case where of switch if then else"

--http://guppy.eng.kagawa-u.ac.jp/2005/AdvProg/Programs/IO.hs
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
                  try ifthenelse <|>
                  try plet <|> 
                  try pcase <|>
                  try pswitch <|>
                  try tyAnno <|>
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
                   pat <- parsePat
                   opTok "->" 
                   e <- expr
                   return $ ELam pat e
                <?> "lambda term"
          tyAnno = do ty <- parseTy
                      opTok ":"
                      e <- expr
                      return $ ETy ty e
                   <?> "type annotation"
          psig = EApp (EVar "sig") `fmap` bracketed Sig expr  <?> "signal expression"
          psigval = EApp (EVar "sigval") `fmap` bracketed SigVal expr  <?> "signal valueexpression"
          unitConstr = do bracketed Parens (return ())
                          return $ ECon (VCons "()" [])
          plet = do tok $ Id "let"
                    ppes <- sepBy1 pletline (tok $ EndOf LetLine)
                    tok $ Id "in"
                    re <- expr
                    return (ELet ppes re)
                    <?> "let term"
          ifthenelse = do tok $ Id "if"
                          pe <- expr
                          tok $ Id "then"
                          ce <- expr
                          tok $ Id "else"
                          ae <- expr
                          return (EVar "if" $> pe $> ce $> ae)
                    <?> "let term"
          pletline = do p <- parsePat
                        opTok "="
                        pe <- expr
                        return (p,pe)
          pcase = do tok $ Id "case"
                     re <- expr
                     tok $ Id "of"
                     ppes <- sepBy1 pcaseline (tok $ EndOf CaseLine)                     
                     return (ECase re ppes )
                     <?> "case term"
          pcaseline = do p <- parsePat
                         opTok "->"
                         pe <- expr
                         return (p,pe)
          pswitch = do tok $ Id "switch"
                       ppes <- sepBy1 pswitchline (tok $ EndOf SwitchLine)                     
                       return (ECase (EVar "_switch") ppes)
                    <?> "switch term"
          pswitchline = do p <- parsePat
                           opTok "~>"
                           pe <- expr
                           return (p,pe)
                       

parsePat :: EParser Pat
parsePat = buildExpressionParser table pPat <?> "Pattern"
    where pPat = (bracketed Parens parsePat)
                 <|> pconstrOrVar
                 <|> (const PWild `fmap` tok Underscore)
                 <|> ((PLit . VInt) `fmap` con_int)
                 <?> "pattern"
          table = [[bang_op]]
          bang_op = Postfix (const (\p-> PBang p) `fmap` opTok "!")
          pconstrOrVar = do nm <- identifier
                            if isLower $ head nm
                               then return $ PVar nm
                               else do cargs <- many pPat
                                       return $ PCons nm cargs
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
                          
parseFixDec :: Parser FixDec
parseFixDec = try pin <|> try ppost <|> ppre <?> "fixity declaration"
    where pin = do string "infix"
                   assoc <- (char 'l' >> return AssocLeft) <|>
                            (char 'r' >> return AssocRight) <|>
                            (char ' ' >> return AssocNone) 
                   spaces
                   n <- number
                   spaces
                   op <- many1 (oneOf opChars)
                   return $ FixDec In assoc n op
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
        isFixDec ('i':'n':'f':'i':'x':lr:_) = lr == 'r' || lr =='l' || lr ==' '
        isFixDec ('p':'r':'e':'f':'i':'x':' ':_) = True
        isFixDec ('p':'o':'s':'t':'f':'i':'x':' ':_) = True
        isFixDec ln = False
        fixDecLns = filter isFixDec lns
        toks = withLayout $ lex 0 0 in_s 
        
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