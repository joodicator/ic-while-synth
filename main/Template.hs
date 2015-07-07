{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Template where

import qualified While
import Clingo
import Logic

import Data.Function
import Data.List

--------------------------------------------------------------------------------
newtype Template
  = Template [TemplatePart]
  deriving Show

data TemplatePart
  = TPCond ConditionType Condition
  | TPInstr TemplateInstr
  | TPBlock BlockType While.Guard Template
  deriving Show

data TemplateInstr
  = TISet While.Var While.Expr
  deriving Show

data ConditionType
  = CTPre | CTPost | CTMid
  deriving Show

data BlockType
  = BTIf | BTWhile
  deriving Show

--------------------------------------------------------------------------------
-- Attempts to extract a template from an ASP specification using template/2.
answerToTemplate :: Answer -> Either String Template
answerToTemplate facts
  = case termsToTemplateN terms of
        Left (n, err)  -> Left $ err ++ ", in: " ++ showFact (facts' !! n)
        Right template -> Right template
  where
    (terms, facts') = unzip . map snd . sortBy (compare `on` fst) $ [
        (n, (term, fact)) | fact@(Fact "template" [TInt n, term]) <- facts]

type NTerm  = (Int, Term)
type NError = (Int, String)

-- As answerToTemplate, but accepts a list of terms taking the same form as
-- the second argument of template/2, and includes an index into this list
-- in any error result.
termsToTemplateN :: [Term] -> Either NError Template
termsToTemplateN terms = do
    Template template <- eTemplate
    let pref = case [() | TPCond CTPre _ <- take 1 template] of {
        [] -> [TPCond CTPre ""]; _ -> [] }
    let suff = case [() | TPCond CTPost _ <- take 1 (reverse template)] of {
        [] -> [TPCond CTPost ""]; _ -> [] }
    return . Template $ pref ++ template ++ suff
  where
    eTemplate = termsToTemplateN' (zip [0..] terms)

    termsToTemplateN' :: [NTerm] -> Either NError Template
    termsToTemplateN' ((n, t) : lines) = case t of
        TFun "pre" [_] | n > 0 -> do
            fail $ "precondition after beginning of template"
        TFun "post" [_] | not (null lines) -> do
            fail $ "postcondition before end of template"
        TFun (conditionType -> Just cType) [cTerm] -> do
            -- pre/1, post/1, mid/1.
            cond <- maybe (fail "invalid condition") Right (termToCond cTerm)
            Template template <- termsToTemplateN' lines
            return . Template $ TPCond cType cond : template
        TFun (blockType -> Just bType) [gTerm] -> do
            -- if/1, while/1, end_if/0, end_while/0.
            guard <- maybe (fail "invalid guard") Right (While.readGuard gTerm)
            (body, tail) <- splitBlock (n, bType) lines
            bTemplate <- termsToTemplateN' body
            Template template <- termsToTemplateN' tail
            return . Template $ TPBlock bType guard bTemplate : template
        (templateInstr -> Just instr) -> do
            -- set/2.
            Template template <- termsToTemplateN' lines
            return . Template $ TPInstr instr : template
        _ ->
            fail $ "unrecognised template entry"
      where
        fail err = Left (n, err)
    termsToTemplateN' [] = Right $ Template []

    splitBlock :: (Int, BlockType) -> [NTerm] -> Either NError ([NTerm], [NTerm])
    splitBlock (m, bType) (l@(n, t) : lines) = case t of
        TFun (blockType -> Just bType') [] -> do
            (body,  tail)   <- splitBlock (n, bType') lines
            (body', tail') <- splitBlock (m, bType) tail
            Right (body ++ body', tail')
        TFun name [] | name == blockEnd bType ->
            Right ([], lines)
        TFun name [] | name `elem` ["end_if", "end_while"] ->
            Left (n, "mismatched " ++ let Name sName = name in sName)
        _ -> do
            (body, tail) <- splitBlock (m, bType) lines
            Right (l : body, tail)
    splitBlock (m, BTIf)    [] = Left (m, "if without end_if")
    splitBlock (m, BTWhile) [] = Left (m, "while without end_while")

--------------------------------------------------------------------------------
templateInstr :: Term -> Maybe TemplateInstr
templateInstr term = case While.readInstr term of
    Just (While.ISet var expr) -> Just $ TISet var expr
    _                          -> Nothing

conditionType :: Name -> Maybe ConditionType
conditionType "pre"  = Just CTPre
conditionType "post" = Just CTPost
conditionType "mid"  = Just CTMid
conditionType _      = Nothing

blockType :: Name -> Maybe BlockType
blockType "if"    = Just $ BTIf
blockType "while" = Just $ BTWhile
blockType _       = Nothing

blockEnd :: BlockType -> Name
blockEnd BTIf    = "end_if"
blockEnd BTWhile = "end_while"
