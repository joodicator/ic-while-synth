{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleContexts, TransformListComp #-}

module ASP.Parse where

import ASP.Base
import ASP.Operator
import Util

import GHC.Exts(sortWith, groupWith)
import Text.Parsec

import Prelude hiding (head)
import Data.Either
import Data.Monoid
import Control.Applicative hiding ((<|>), many)
import Control.Monad

type Parse s u m a = Stream s m Char => ParsecT s u m a

opInfo' :: (Operator a, OpContext a ~ ASPContext) => a -> OpInfo
opInfo' = opInfo Clingo3

--------------------------------------------------------------------------------
-- Parsing of ASP structures.

rule :: Parse s u m Rule
rule = (mempty :-) <$> (pre ":-" *> (mempty <$ char '.' <|> body <* suf "."))
   <|> (:-) <$> head <*> (inf ":-" *> (body <* suf "." <|> mempty <$ char '.')
   <|> mempty <$ suf ".")

head :: Parse s u m Head
head = Head <$> sepBy1 atom (try $ inf "|")

body :: Parse s u m Body
body = Body <$> sepBy1 conjunct (inf ",")

atom :: Parse s u m Atom
atom = Atom <$> lName Predicate <*> args expr

conjunct :: Parse s u m Conjunct
conjunct = CLiteral   <$> literal
       <|> CAssign    <$> variable <* inf "=" <*> expr
       <|> CCondition <$> literal  <* inf ":" <*> conjunct

literal :: Parse s u m Literal
literal = try (LCompare <$> comparison)
      <|> LAtom    <$> atom
      <|> LNot     <$> (string "not" *> many1 space *> atom)

comparison :: Parse s u m Comparison
comparison = flip CBiOp <$> expr <* spaces <*> cBiOp <* spaces <*> expr

cBiOp :: Parse s u m CBiOp
cBiOp = CEq <$ string "==" <|> CNE <$ string "!="
    <|> CLE <$ string "<=" <|> CGT <$ string ">"
    <|> CGE <$ string ">=" <|> CLT <$ string "<"

expr :: Parse s u m Expr
expr = expr' oss where
    oss :: [[Either EUnOp EBiOp]]
    oss = [o | o <- map Left  [minBound::EUnOp ..]
                 ++ map Right [minBound::EBiOp ..],
               let p = oPrec $ either opInfo' opInfo' o,
               then sortWith by p, then group by p using groupWith]

    expr' :: [[Either EUnOp EBiOp]] -> Parse s u m Expr
    expr' [] = choice
       [between (pre "(") (suf ")") expr,
        EUnOp EAbs <$> between (pre "|") (suf "|") expr,
        ETerm <$> term]
    expr' oss'@(os@(_:_) : _) = choice
       [exprUn (lefts  os) oss',
        exprBi (rights os) oss']
    expr' ([] : _) = error "impossible"

    exprUn :: [EUnOp] -> [[Either EUnOp EBiOp]] -> Parse s u m Expr
    exprUn os oss' = do
        o <- choice $ map eUnOp os
        let OpInfo{ oAscL=ascL } = opInfo' o
        x <- expr' $ if ascL then oss' else drop 1 oss'
        return $ EUnOp o x

    exprBi :: [EBiOp] -> [[Either EUnOp EBiOp]] -> Parse s u m Expr
    exprBi os oss' = do
        x  <- expr' $ drop 1 oss'
        mo <- optionMaybe . choice $ map eBiOp os
        case mo of
            Just o | oAscL (opInfo' o) -> exprAscL x o os oss'
            Just o | oAscR (opInfo' o) -> EBiOp o x <$> expr' oss'
            Just o | otherwise         -> EBiOp o x <$> expr' (drop 1 oss')
            Nothing                    -> return x

    exprAscL
       :: Expr -> EBiOp -> [EBiOp] -> [[Either EUnOp EBiOp]] -> Parse s u m Expr
    exprAscL x o os oss' = do
        x' <- EBiOp o x <$> expr' (drop 1 oss')
        mo <- optionMaybe . choice $ map eBiOp os
        case mo of
            Just o' | oAscL (opInfo' o) -> exprAscL x' o' os oss'
            Just _  | otherwise        -> fail "ambiguous arithmetic expression"
            Nothing                    -> return x'

eUnOp :: EUnOp -> Parse s u m EUnOp
eUnOp op = case op of
    EAbs -> op <$ try (string "#abs" *> notFollowedBy (alphaNum <|> char '_'))
    ENeg -> op <$ char '-'
    ENot -> op <$ char '~'

eBiOp :: EBiOp -> Parse s u m EBiOp
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

term :: Parse s u m Term
term = TVar <$> variable
   <|> TInt <$> intTerm
   <|> TStr <$> strTerm
   <|> lName (TFun . Function) <*> args expr
   <|> char '@' *> lName (TLua . LuaFunc) <*> args expr

intTerm :: Parse s u m Integer
intTerm = read <$> many1 digit

strTerm :: Parse s u m String
strTerm = between (char '"') (char '"')
          (many $ char '\\' *> char '"' <|> noneOf "\"\\")

--------------------------------------------------------------------------------
-- Parsing of low-level ASP tokens:

variable :: Parse s u m Variable
variable = fmap Variable $ (:) <$> (upper <|> char '_')
                               <*> many (alphaNum <|> char '_')

-- The name of a predicate, function symbol or constant.
lName :: (String -> a) -> Parse s u m a
lName con = fmap con $ (:) <$> lower <*> many (alphaNum <|> char '_')

-- A parenthesised list of 1 or more arguments, or the empty string.
args :: Parse s u m a -> Parse s u m [a]
args arg = option [] . between (pre "(") (suf ")") $ sepBy1 arg (inf ",")

-- Infix operators (may follow or be followed by space):
inf :: String -> Parse s u m ()
inf op = void $ spaces *> string op *> spaces

-- Prefix operators (may be followed by space):
pre :: String -> Parse s u m ()
pre op = void $ string op *> spaces

-- Suffix operators (may follow space):
suf :: String -> Parse s u m ()
suf op = void $ spaces *> string op
