{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts,
             FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses, OverloadedStrings, TransformListComp #-}

module ASP.Parse where

import ASP.Base
import ASP.Operator
import Util

import GHC.Exts(sortWith, groupWith)
import Text.Parsec

import Prelude hiding (head)
import Data.ByteString (ByteString)
import Data.Either
import Data.Monoid
import Data.Functor.Identity
import Control.Applicative hiding ((<|>), many)
import Control.Monad

opInfo' :: (Operator a, OpContext a ~ ASPContext) => a -> OpInfo
opInfo' = opInfo Clingo3

--------------------------------------------------------------------------------
-- Parsing of ASP structures.

class ParseASP s u m where
    rule       :: ParsecT s u m Rule
    head       :: ParsecT s u m Head
    body       :: ParsecT s u m Body
    atom       :: ParsecT s u m Atom
    conjunct   :: ParsecT s u m Conjunct
    literal    :: ParsecT s u m Literal
    comparison :: ParsecT s u m Comparison
    expr       :: ParsecT s u m Expr
    cBiOp      :: ParsecT s u m CBiOp
    eUnOp      :: EUnOp -> ParsecT s u m EUnOp
    eBiOp      :: EBiOp -> ParsecT s u m EBiOp
    term       :: ParsecT s u m Term
    intTerm    :: ParsecT s u m Integer
    strTerm    :: ParsecT s u m String
    variable   :: ParsecT s u m Variable
    lName      :: (String -> a) -> ParsecT s u m a
    args       :: ParsecT s u m a -> ParsecT s u m [a]
    inf        :: String -> ParsecT s u m ()
    pre        :: String -> ParsecT s u m ()
    suf        :: String -> ParsecT s u m ()

instance (Monad m, Stream s m Char) => ParseASP s u m where
    {-# SPECIALISE instance ParseASP String () Identity #-}

    rule = (mempty :-) <$> (pre ":-" *> (mempty <$ char '.' <|> body <* suf "."))
       <|> (:-) <$> head <*> (inf ":-" *> (body <* suf "." <|> mempty <$ char '.')
       <|> mempty <$ suf ".")
        
    head = Head <$> sepBy1 atom (try $ inf "|")
    
    body = Body <$> sepBy1 conjunct (inf ",")
    
    atom = Atom <$> lName Predicate <*> args expr
    
    conjunct = CLiteral   <$> literal
           <|> CAssign    <$> variable <* inf "=" <*> expr
           <|> CCondition <$> literal  <* inf ":" <*> conjunct
    
    literal = LCompare <$> try comparison
          <|> LNot     <$> (try (string "not" *> many1 space) *> atom)
          <|> LAtom    <$> atom
    
    comparison = flip CBiOp <$> expr <* spaces <*> cBiOp <* spaces <*> expr
    
    cBiOp = CEq <$ string "==" <|> CNE <$ string "!="
        <|> CLE <$ string "<=" <|> CGT <$ string ">"
        <|> CGE <$ string ">=" <|> CLT <$ string "<"
    
    expr = expr' oss where
        --oss :: [[Either EUnOp EBiOp]]
        oss = [o | o <- map Left  [minBound::EUnOp ..]
                     ++ map Right [minBound::EBiOp ..],
                   let p = oPrec $ either opInfo' opInfo' o,
                   then sortWith by p, then group by p using groupWith]
    
        --expr' :: [[Either EUnOp EBiOp]] -> ParsecT s u m Expr
        expr' [] = choice
           [try $ between (pre "(") (suf ")") expr,
            EUnOp EAbs <$> between (pre "|") (suf "|") expr,
            ETerm <$> term]
        expr' oss'@(os@(_:_) : _) = choice
           [exprUn (lefts  os) oss',
            exprBi (rights os) oss']
        expr' ([] : _) = error "impossible"
    
        --exprUn :: [EUnOp] -> [[Either EUnOp EBiOp]] -> ParsecT s u m Expr
        exprUn os oss' = do
            o <- choice $ map eUnOp os
            let OpInfo{ oAscL=ascL } = opInfo' o
            x <- expr' $ if ascL then oss' else drop 1 oss'
            return $ EUnOp o x
    
        --exprBi :: [EBiOp] -> [[Either EUnOp EBiOp]] -> ParsecT s u m Expr
        exprBi os oss' = do
            x  <- expr' $ drop 1 oss'
            mo <- optionMaybe . choice $ map eBiOp os
            case mo of
                Just o | oAscL (opInfo' o) -> exprAscL x o os oss'
                Just o | oAscR (opInfo' o) -> EBiOp o x <$> expr' oss'
                Just o | otherwise         -> EBiOp o x <$> expr' (drop 1 oss')
                Nothing                    -> return x
    
        --exprAscL
        --   :: Expr -> EBiOp -> [EBiOp] -> [[Either EUnOp EBiOp]] -> ParsecT s u m Expr
        exprAscL x o os oss' = do
            x' <- EBiOp o x <$> expr' (drop 1 oss')
            mo <- optionMaybe . choice $ map eBiOp os
            case mo of
                Just o' | oAscL (opInfo' o) -> exprAscL x' o' os oss'
                Just _  | otherwise        -> fail "ambiguous arithmetic expression"
                Nothing                    -> return x'
    
    eUnOp op = case op of
        EAbs -> op <$ try (string "#abs" *> notFollowedBy (alphaNum <|> char '_'))
        ENeg -> op <$ char '-'
        ENot -> op <$ char '~'
    
    eBiOp op = case op of
        EPow -> op <$ try (string "**")
        EMod -> op <$ char '\\'
        EDiv -> op <$ char '/'
        EMul -> op <$ char '*'
        ESub -> op <$ char '-'
        EAdd -> op <$ char '+'
        EAnd -> op <$ char '&'
        EOr  -> op <$ char '?'
        EXOr -> op <$ char '^'
    
    term = TVar <$> variable
       <|> TInt <$> intTerm
       <|> TStr <$> strTerm
       <|> option (TFun "") (lName $ TFun . Function) <*> args expr
       <|> char '@' *> lName (TLua . LuaFunc) <*> args expr
    
    intTerm = read <$> many1 digit
    
    strTerm = between (char '"') (char '"')
              (many $ char '\\' *> char '"' <|> noneOf "\"\\")

    variable = fmap Variable $ (:) <$> (upper <|> char '_')
                                   <*> many (alphaNum <|> char '_')
    
    -- The name of a predicate, function symbol or constant.
    lName con = fmap con $ (:) <$> lower <*> many (alphaNum <|> char '_')
    
    -- A parenthesised list of 1 or more arguments, or the empty string.
    args arg = option [] . between (pre "(") (suf ")") $ sepBy1 arg (inf ",")
    
    -- Infix operators (may follow or be followed by space):
    inf op = void $ spaces *> string op *> spaces
    
    -- Prefix operators (may be followed by space):
    pre op = void $ string op *> spaces
    
    -- Suffix operators (may follow space):
    suf op = void $ spaces *> string op
