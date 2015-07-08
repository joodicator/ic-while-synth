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
data Template = Template {
    teConf  :: TemplateConf,
    teParts :: [TemplatePart] }
  deriving Show

data TemplateConf = TemplateConf{
    tcProgramVars :: [Variable],
    tcLogicVars   :: [Variable] }
  deriving Show

emptyTemplate = Template{
    teConf  = defaultTemplateConf,
    teParts = [] }

defaultTemplateConf = TemplateConf{
    tcProgramVars = [],
    tcLogicVars   = [] }

data TemplatePart
  = TPCond ConditionType Condition
  | TPInv Invariant
  | TPVar Variant
  | TPInstr TemplateInstr
  | TPBlock BlockType While.Guard [TemplatePart]
  deriving Show

data Invariant
  = Invariant Condition
  deriving Show

data Variant
  = VarVariant Variable Condition VariantDirection
  | ExpVariant Condition VariantDirection
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

-- Values annotated with a line number:
type NTerm  = (Int, Term)
type NError = (Int, String)

--------------------------------------------------------------------------------
-- Reads a template from the ASP at the given file path.
fileTemplate :: FilePath -> IO Template
fileTemplate path = do
    let options = runClingoOptions{ rcEchoStdout=False, rcEchoInput=False }
    runClingo options [CIFile path] >>= \result -> case result of
        CRSatisfiable (answer : _) ->
            either error return $ answerToTemplate answer
        _ ->
            error "unsatisfiable"

--------------------------------------------------------------------------------
-- Attempts to extract from an ASP specification a template that is VALID, i.e.
-- 1. No precondition occurs after the beginning of the template.
-- 2. No postcondition occurs after the end of the template.
-- 3. No loop variant or invariant occurs outside of a while loop.
answerToTemplate :: Answer -> Either String Template
answerToTemplate facts
  = case termsToTemplateN terms of
      Right parts ->
        Right $ Template {
            teConf = TemplateConf{
                tcProgramVars = programVars,
                tcLogicVars   = logicVars },
            teParts = parts }
      Left (n, err) ->
        Left $ err ++ ", in: " ++ showFact (facts' !! n)
  where
    programVars = [
        var | Fact "program_variable" [TFun (Name var) []] <- facts]
    logicVars = [
        var | Fact "logic_variable" [TFun (Name var) []] <- facts]
    (terms, facts') = unzip . map snd . sortBy (compare `on` fst) $ [
        (n, (term, fact)) | fact@(Fact "template" [TInt n, term]) <- facts]

-- A partial form of answerToTemplate that takes a list of terms of the same
-- form as the second argument of template/2 and returns a list of template
-- elements or an error string annotated with an index into the input list.
termsToTemplateN :: [Term] -> Either NError [TemplatePart]
termsToTemplateN terms
  = termsToTemplateN' Nothing (zip [0..] terms)
  where
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
            (TPVar (VarVariant var con dir) :) <$> termsToTemplateN' parent lines
        TFun "var" [eTerm, dTerm] -> do
            -- Loop variant given by expression.
            -- var/2.
            exp <- maybe (fail "invalid expression") Right (termToCond eTerm)
            dir <- maybe (fail "invalid variant direction") Right $ do
                TFun dName [] <- return dTerm; variantDirection dName
            (TPVar (ExpVariant exp dir) :) <$> termsToTemplateN' parent lines
        TFun (blockType -> Just bType) [gTerm] -> do
            -- If/while block.
            -- if/1, while/1, end_if/0, end_while/0.
            guard <- maybe (fail "invalid guard") Right (While.readGuard gTerm)
            (body, tail) <- splitBlock (n, bType) lines
            bTemplate <- termsToTemplateN' (Just bType) body
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
normaliseTemplate :: Template -> Template
normaliseTemplate template@Template{ teParts=parts, teConf=conf }
  = template{ teParts=parts'', teConf=conf' }
  where
    (parts', conf') = normaliseSubTemplate parts conf
    parts'' = ensurePrePost parts'
    
    -- Enforces property (1) of on a template.
    ensurePrePost :: [TemplatePart] -> [TemplatePart]
    ensurePrePost ps | null [() | TPCond CTPre _ <- take 1 ps]
      = ensurePrePost $ [TPCond CTPre ""] ++ ps
    ensurePrePost ps | null [() | TPCond CTPost _ <- take 1 $ reverse ps]
      = ensurePrePost $ ps ++ [TPCond CTPost ""]
    ensurePrePost ps = ps

-- Enforces property (2) of normaliseTemplate on a subtemplate,
-- possibly changing the global template configuration.
normaliseSubTemplate
  :: [TemplatePart] -> TemplateConf -> ([TemplatePart], TemplateConf)
normaliseSubTemplate (part : parts) conf = case part of
    TPBlock BTWhile guard body ->
        let (repl, conf') = expandWhileTemplate (guard, body) conf in
        let (parts', conf'') = normaliseSubTemplate parts conf' in
        (repl ++ parts', conf'')
    TPBlock bType guard body ->
        let (body', conf') = normaliseSubTemplate body conf in
        let (parts', conf'') = normaliseSubTemplate parts' conf' in
        (TPBlock bType guard body' : parts', conf'')
    _ ->
        let (parts', conf') = normaliseSubTemplate parts conf in
        (part : parts', conf')
normaliseSubTemplate [] conf
  = ([], conf)

-- Expands a while loop given by its guard and the subtemplate in its body,
-- according to property (2) of normaliseTemplate, giving a subtemplate to replace
-- the whole loop, and possibly changing the global template configuration.
expandWhileTemplate
  :: (While.Guard, [TemplatePart]) -> TemplateConf -> ([TemplatePart], TemplateConf)
expandWhileTemplate (guard, body) conf
  = (expanded, conf'')
  where
    expanded = [
        TPCond CTMid invCond,
        TPBlock BTWhile guard $
            [TPCond CTMid $ invCond ++", "++ vPreCond ++", "++ guardToCond guard]
            ++ body'' ++
            [TPCond CTMid $ invCond ++", "++ vPostCond],
        TPCond CTMid $ invCond ++", "++ guardToCond (While.GNeg guard)]

    (invariants, variants, body') = splitParts body
    conf' = conf{ tcLogicVars = newLogicVars ++ tcLogicVars conf }
    (body'', conf'') = normaliseSubTemplate body' conf'

    invCond   = intercalate ", " [c | Invariant c <- invariants]
    vPreCond  = intercalate ", " vPreConds
    vPostCond = intercalate ", " vPostConds
    (unzip -> (vPreConds, vPostConds), concat -> newLogicVars)
      = unzip $ vVarConds variants unusedVars

    unusedVars = ["v" ++ show i | i <- [0..]] \\ usedVars
    usedVars   = globalVars ++ iLocalVars ++ vLocalVars
    globalVars = tcProgramVars conf ++ tcLogicVars conf
    iLocalVars = headMap toLower <$> freeVariables invCond \\ globalVars
    vLocalVars = (\\ globalVars) . map (headMap toLower) . nub . sort $
        concat [freeVariables c \\ [v] | (v,c,_) <- normVariant <$> variants]

    -- Given a list of variants and an (infinite) list of unused variables,
    -- gives a list of ((p,q),vs), where p is a variant's contribution to the
    -- precondition of the loop body, q is its contribution to the postcondition,
    -- and vs are the previously unused variables that are now logic variables.
    vVarConds :: [Variant] -> [Variable] -> [((Condition,Condition), [Variable])]
    vVarConds (variant : vs) (uVar : uVar' : us)
      = ((preCond,postCond++", "++varCond), [uVar,uVar']) : vVarConds vs us
      where
        preCond  = subFreeVariable (headUp vVar) (headUp uVar) cond
        postCond = subFreeVariable (headUp vVar) (headUp uVar') cond
        varCond  = headUp uVar' ++ rel ++ headUp uVar
        rel = case dir of VDIncreasing -> " > "; VDDecreasing -> " < " 
        (vVar, cond, dir) = normVariant variant
    vVarConds [] _ = []
  
    -- Normalises a Variant into a three-parameter form.
    normVariant :: Variant -> (Variable, Condition, VariantDirection)
    normVariant (VarVariant var con dir)
      = (var, con, dir)
    normVariant (ExpVariant exp dir)
      = (var, headMap toUpper var ++ " = " ++ exp, dir)
      where var = head $ ["v" ++ show i | i <- [0..]] \\ freeVariables exp

    -- Removes all invariants and variants from the top level of a (sub)template,
    -- returning them along with the remaining template.
    splitParts :: [TemplatePart] -> ([Invariant], [Variant], [TemplatePart])
    splitParts = foldr f ([], [], []) where
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
