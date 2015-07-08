{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Template where

import qualified While
import Clingo
import Logic

import Data.Function
import Data.List
import Data.Char
import Control.Applicative
import Control.Monad

--------------------------------------------------------------------------------
newtype Template
  = Template [TemplatePart]
  deriving Show

data TemplatePart
  = TPCond ConditionType Condition
  | TPInv Invariant
  | TPVar Variant
  | TPInstr TemplateInstr
  | TPBlock BlockType While.Guard Template
  deriving Show

data Invariant
  = Invariant Condition
  deriving Show

data Variant
  = Variant Variable Condition VariantDirection
  deriving Show

data TemplateInstr
  = TISet While.Var While.Expr
  deriving Show

data ConditionType
  = CTPre | CTPost | CTMid
  deriving Show

data BlockType
  = BTIf | BTWhile
  deriving (Eq, Show)

data VariantDirection
  = VDIncreasing | VDDecreasing
  deriving Show

--------------------------------------------------------------------------------
-- Attempts to extract from an ASP specification a template that is VALID, i.e.
-- 1. No precondition occurs after the beginning of the template.
-- 2. No postcondition occurs after the end of the template/
-- 3. No loop variant or invariant occurs outside of a while loop.
answerToTemplate :: Answer -> Either String Template
answerToTemplate facts
  = case termsToTemplateN terms of
        Left (n, err)  -> Left $ err ++ ", in: " ++ showFact (facts' !! n)
        Right template -> Right template
  where
    (terms, facts') = unzip . map snd . sortBy (compare `on` fst) $ [
        (n, (term, fact)) | fact@(Fact "template" [TInt n, term]) <- facts]

-- Reads a template from the ASP at the given file path.
readTemplate :: FilePath -> IO Template
readTemplate path = do
    let options = runClingoOptions{ rcEchoStdout=False, rcEchoInput=False }
    runClingo options [CIFile path] >>= \result -> case result of
        CRSatisfiable (answer : _) ->
            either error return $ answerToTemplate answer
        _ ->
            error "unsatisfiable"

-- As answerToTemplate, but accepts a list of terms taking the same form as
-- the second argument of template/2, and includes an index into this list
-- in any error result.
type NTerm  = (Int, Term)
type NError = (Int, String)

termsToTemplateN :: [Term] -> Either NError Template
termsToTemplateN terms = do
    Template template <- eTemplate
    let pref = case [() | TPCond CTPre _ <- take 1 template] of {
        [] -> [TPCond CTPre ""]; _ -> [] }
    let suff = case [() | TPCond CTPost _ <- take 1 (reverse template)] of {
        [] -> [TPCond CTPost ""]; _ -> [] }
    return . Template $ pref ++ template ++ suff
  where
    eTemplate = Template <$> termsToTemplateN' Nothing (zip [0..] terms)

    -- Parses a list of (index, term) pairs representing part of a template, given
    -- Just the parent block type of this subtemplate, or Nothing if at top-level.
    termsToTemplateN' :: Maybe BlockType -> [NTerm] -> Either NError [TemplatePart]
    termsToTemplateN' parent ((n, t) : lines) = case t of
        TFun "pre" [_] | n > 0 -> do
            -- Precondition in illegal position.
            fail $ "precondition after beginning of template"
        TFun "post" [_] | not (null lines) -> do
            -- Postcondition in illegal position.
            fail $ "postcondition before end of template"
        TFun (conditionType -> Just cType) [cTerm] -> do
            -- Preconditions, postconditions, midconditions.
            -- pre/1, post/1, mid/1.
            cond <- maybe (fail "invalid condition") Right (termToCond cTerm)
            (TPCond cType cond :) <$> termsToTemplateN' parent lines
        TFun "inv" [cTerm] -> do
            -- Loop invariant.
            -- inv/1.
            unless (parent == Just BTWhile) $ fail "loop invariant not inside a loop"
            cond <- maybe (fail "invalid invariant condition") Right (termToCond cTerm)
            (TPInv (Invariant cond) :) <$> termsToTemplateN' parent lines
        TFun "var" [vTerm, cTerm, dTerm] -> do
            -- Loop variant given by condition.
            -- var/3.
            unless (parent == Just BTWhile) $ fail "loop variant not inside a loop"
            var <- maybe (fail "invalid variant variable") Right $ do
                TFun (Name vName) [] <- return vTerm; return vName
            con <- maybe (fail "invalid variant condition") Right $ termToCond cTerm
            dir <- maybe (fail "invalid variant direction") Right $ do
                TFun dName [] <- return dTerm; variantDirection dName
            (TPVar (Variant var con dir) :) <$> termsToTemplateN' parent lines
        TFun "var" [eTerm, dTerm] -> do
            -- Loop variant given by expression.
            -- var/2.
            exp <- maybe (fail "invalid expression") Right (termToCond eTerm)
            dir <- maybe (fail "invalid variant direction") Right $ do
                TFun dName [] <- return dTerm; variantDirection dName
            let vars = "v" : ["v" ++ show i | i <- [0..]]
            let var = head . filter (not . (`isFreeIn` exp)) $ vars
            let con = headMap toUpper var ++ " = " ++ exp
            (TPVar (Variant var con dir) :) <$> termsToTemplateN' parent lines
        TFun (blockType -> Just bType) [gTerm] -> do
            -- If/while block.
            -- if/1, while/1, end_if/0, end_while/0.
            guard <- maybe (fail "invalid guard") Right (While.readGuard gTerm)
            (body, tail) <- splitBlock (n, bType) lines
            bTemplate <- Template <$> termsToTemplateN' (Just bType) body
            (TPBlock bType guard bTemplate :) <$> termsToTemplateN' parent tail
        (templateInstr -> Just instr) -> do
            -- Non-block instruction.
            -- set/2.
            (TPInstr instr :) <$> termsToTemplateN' parent lines
        _ ->
            fail $ "unrecognised template entry"
      where
        fail err = Left (n, err)
    termsToTemplateN' _ [] = Right []

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
-- Given a VALID template, gives an equivalent NORMAL template, i.e. one with:
-- 1. A pre- and postcondition (defaulting to the empty (true) condition).
-- 2. No loop variants or invariants (instead, corresponding midconditions).
expandTemplate :: Template -> Template
expandTemplate (Template parts)
  = Template $ ensurePrePost . expandLoops $ parts
  where
    -- Enforces property (1) on a template.
    ensurePrePost :: [TemplatePart] -> [TemplatePart]
    ensurePrePost ps | null [() | TPCond CTPre _ <- take 1 ps]
      = ensurePrePost $ [TPCond CTPre ""] ++ ps
    ensurePrePost ps | null [() | TPCond CTPost _ <- take 1 $ reverse ps]
      = ensurePrePost $ ps ++ [TPCond CTPost ""]
    ensurePrePost ps = ps

    -- Enforces property (2) on a (sub)template.
    expandLoops :: [TemplatePart] -> [TemplatePart]
    expandLoops = concatMap $ \p -> case p of
        TPBlock BTWhile guard body -> expandWhile (guard, body)
        TPBlock bType   guard body -> [TPBlock bType guard $ expandLoops body]
        _                          -> [p]

    -- Given the guard and body-template of a while loop, returns a NORMAL
    -- subtemplate equivalent to this loop.
    expandWhile :: (Guard, [TemplatePart]) -> [TemplatePart]
    expandWhile (guard, body)
      = error "expandTemplate.expandWhile: not implemented"
      where
        (invs, vars, body') = invariants body
        inv     = intercalate ", " invs

    invariants :: [TemplatePart] -> ([Invariant], [Variant], [TemplatePart])
    invariants = foldr' f ([], [], []) where
        f (TPInv i) (is, vs, ps) = (i:is, vs, ps)
        f (TPVar v) (is, vs, ps) = (is, v:vs, ps)
        f p         (is, vs, ps) = (is, vs, p:ps)

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
blockType "if"    = Just BTIf
blockType "while" = Just BTWhile
blockType _       = Nothing

variantDirection :: Name -> Maybe VariantDirection
variantDirection "inc"        = Just VDIncreasing
variantDirection "increasing" = Just VDIncreasing
variantDirection "dec"        = Just VDDecreasing
variantDirection "decreasing" = Just VDDecreasing
variantDirection _            = Nothing

blockEnd :: BlockType -> Name
blockEnd BTIf    = "end_if"
blockEnd BTWhile = "end_while"
