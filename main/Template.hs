{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Template where

import qualified While
import Clingo
import Logic

import Data.Function
import Data.List
import Data.Char
import Data.MonoTraversable
import Control.Monad
import Control.Applicative

--------------------------------------------------------------------------------
-- The Template data type and its components:

data Template = Template {
    teConf  :: TeConf,
    teParts :: [TePart] }
  deriving Show

data TeConf = TeConf{
    tcProgramVars :: [Variable],
    tcLogicVars   :: [Variable],
    tcReadOnly    :: [Variable] }
  deriving Show

emptyTemplate = Template{
    teConf  = defaultTeConf,
    teParts = [] }

defaultTeConf = TeConf{
    tcProgramVars = [],
    tcLogicVars   = [],
    tcReadOnly    = [] }

data TePart
  = TPCond ConditionType Condition
  | TPInv Invariant
  | TPVar Variant
  | TPInstr TemplateInstr
  | TPBlock BlockType While.Guard [TePart]
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
-- Attempts to extract from an ASP specification a template that is VALID, i.e.
-- 1. No precondition occurs after the beginning of the template.
-- 2. No postcondition occurs after the end of the template.
-- 3. No loop variant or invariant occurs outside of a while loop.
answerToTemplate :: Answer -> Either String Template
answerToTemplate facts
  = case termsToTemplateN terms of
      Right parts ->
        Right $ Template {
            teConf = TeConf{
                tcProgramVars = programVars,
                tcLogicVars   = logicVars,
                tcReadOnly    = readOnly },
            teParts = parts }
      Left (n, err) ->
        Left $ err ++ " in " ++ showFact (facts' !! n)
  where
    programVars = [
        var | Fact "program_variable"   [TFun (Name var) []] <- facts]
    logicVars = [
        var | Fact "logic_variable"     [TFun (Name var) []] <- facts]
    readOnly = [
        var | Fact "read_only_variable" [TFun (Name var) []] <- facts]
    (terms, facts') = unzip . map snd . sortBy (compare `on` fst) $ [
        (n, (term, fact)) | fact@(Fact "template" [TInt n, term]) <- facts]

-- A partial form of answerToTemplate that takes a list of terms of the same
-- form as the second argument of template/2 and returns a list of template
-- elements or an error string annotated with an index into the input list.
termsToTemplateN :: [Term] -> Either NError [TePart]
termsToTemplateN terms
  = termsToTemplateN' Nothing (zip [0..] terms)
  where
    -- Parses a list of (index, term) pairs representing part of a template, given
    -- Just the parent block type of this subtemplate, or Nothing if at top-level.
    termsToTemplateN' :: Maybe BlockType -> [NTerm] -> Either NError [TePart]
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
            let body' = reverse . drop 1 . reverse $ body
            bTemplate <- termsToTemplateN' (Just bType) body'
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
        TFun (blockType -> Just bType') _ -> do
            (body,  tail)  <- splitBlock (n, bType') lines
            (body', tail') <- splitBlock (m, bType) tail
            Right (l : body ++ body', tail')
        TFun name [] | name == blockEnd bType ->
            Right ([l], lines)
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
    ensurePrePost :: [TePart] -> [TePart]
    ensurePrePost ps | null [() | TPCond CTPre _ <- take 1 ps]
      = ensurePrePost $ [TPCond CTPre ""] ++ ps
    ensurePrePost ps | null [() | TPCond CTPost _ <- take 1 $ reverse ps]
      = ensurePrePost $ ps ++ [TPCond CTPost ""]
    ensurePrePost ps = ps

-- Enforces property (2) of normaliseTemplate on a subtemplate,
-- possibly changing the global template configuration.
normaliseSubTemplate
  :: [TePart] -> TeConf -> ([TePart], TeConf)
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
expandWhileTemplate :: (While.Guard, [TePart]) -> TeConf -> ([TePart], TeConf)
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
    vLocalVars = (\\ globalVars) . nub . sort . concat $ [
        map (headMap toLower) (freeVariables c) \\ [v]
        | (v,c,_) <- normVariant <$> variants]

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
        rel = case dir of VDIncreasing -> ">"; VDDecreasing -> "<" 
        (vVar, cond, dir) = normVariant variant
    vVarConds [] _ = []
    vVarConds _  _ = error "impossible"
  
    -- Normalises a Variant into a three-parameter form.
    normVariant :: Variant -> (Variable, Condition, VariantDirection)
    normVariant (VarVariant var con dir)
      = (var, con, dir)
    normVariant (ExpVariant exp dir)
      = (var, headMap toUpper var ++ "=" ++ exp, dir)
      where var = head $ ["v" ++ show i | i <- [0..]] \\ freeVariables exp

    -- Removes all invariants and variants from the top level of a (sub)template,
    -- returning them along with the remaining template.
    splitParts :: [TePart] -> ([Invariant], [Variant], [TePart])
    splitParts = foldr f ([], [], []) where
        f (TPInv i) (is, vs, ps) = (i:is, vs, ps)
        f (TPVar v) (is, vs, ps) = (is, v:vs, ps)
        f p         (is, vs, ps) = (is, vs, p:ps)

--------------------------------------------------------------------------------
type Parent = Condition
type Child  = Condition

-- For each condition in the template possibly occurring as the precondition
-- of a program fragment, amends it with information from conditions definitely
-- having held earlier in program execution, such that any constraints on global
-- immutable variables is propagated to all parts of the program.
completeTemplate :: Template -> Template
completeTemplate template@Template{ teConf=conf, teParts=parts }
  = template{ teConf=conf', teParts=parts' }
  where
    (parts', conf', _) = cmpSubTmpl parts conf []

    -- Make a subtemplate complete with respect to a list of "parent" condition
    -- fragments to be appended to each child in addition to any further parents
    -- generated inside this subtemplate, returning a list of "child"
    -- conditions as they were before being modified, and transforming some
    -- global template configuration.
    cmpSubTmpl :: [TePart] -> TeConf -> [Parent] -> ([TePart], TeConf, [Child])
    cmpSubTmpl parts conf parents = case parts of
        TPCond ct cond : tail ->
            (TPCond ct cond'' : tail', conf'', children') where
            (cond', parents', conf') = case children of
                _:_ ->
                    (cond', parents', conf') where
                    (cond', parents', conf') = newParent cond parents children conf
                []  -> (cond, parents, conf)
            (cond'', children') = case tail of
                TPCond{}:_ -> (intercalate ", " $ cond':parents, cond:children)
                _          -> (cond', children)
            (tail', conf'', children) = cmpSubTmpl tail conf' parents'
        TPBlock bt gd body : tail ->
            (TPBlock bt gd body' : tail', conf'', children ++ children') where
            (body', conf',  children)  = cmpSubTmpl body conf  parents
            (tail', conf'', children') = cmpSubTmpl tail conf' parents
        part : tail ->
            (part : tail', conf', children) where
            (tail', conf', children) = cmpSubTmpl tail conf parents
        [] -> ([], conf, [])

    -- Generate a "parent" condition fragment from the given condition, with
    -- respect to the conditions own parents and its child conditions, returning
    -- the parent list possibly prepended with the parent, and transforming both
    -- the condition and some global template configruation.    
    newParent
      :: Condition -> [Parent] -> [Child] -> TeConf -> (Condition, [Parent], TeConf)
    newParent cond parents children conf
      = case parentConjs of
            _:_ -> (cond', parents', conf')
            []  -> (cond,  parents,  conf)
      where
        cond'    = intercalate ", " $ cond : localConjs
        conf'    = conf{ tcLogicVars = localVars' ++ logicVars }
        parents' = parentCond' : parents

        -- Create logic variables to record the values of relevant mutable variables
        -- at this point in time, and conjuncts to fix their values.
        localConjs  = [v'++"="++v | (v,v') <- varPairs]
        parentCond' = subFreeVariables varPairs parentCond
        varPairs    = [(headUp v,headUp v') | (v,v') <- zip localVars localVars']
        localVars'  = map unusedVar localVars
        localVars   = map headLow (freeVariables parentCond) \\ globalVars
        parentCond  = intercalate ", " parentConjs

        -- Discard any conjuncts not relevant to variables used by children.
        parentConjs  = transitiveClosure cGlobalConjs
        cGlobalConjs = filter (\c -> any (`isFreeIn` c) cGlobalVars) allConjs
        allConjs     = otoList $ Conjuncts cond

        -- Determine which global immutable variables are used by any children,
        -- and not already fixed by an earlier parent.
        cGlobalVars = globalVars `intersect` (childVars \\ pParentVars)
        globalVars  = logicVars ++ (programVars `intersect` readOnly)
        pParentVars = map headLow . nub . sort $ concatMap freeVariables parents

        -- Determine which variables already exist in this context.
        unusedVar v = head $ (map ((v ++) . show) [0..]) \\ usedVars
        usedVars    = foldr1 union [programVars, logicVars, childVars, condVars]
        childVars   = map headLow . nub . sort $ concatMap freeVariables children
        condVars    = map headLow $ freeVariables cond

        -- Two conjuncts are related if they share any free variables.
        transitiveClosure conjs
          | null new  = conjs
          | otherwise = transitiveClosure $ new ++ conjs
          where
            new = [c | c <- allConjs \\ conjs, any (related c) conjs]
            related c c' = any (`isFreeIn` c) (freeVariables c')
        
        TeConf{ tcLogicVars=logicVars, tcProgramVars=programVars,
                tcReadOnly=readOnly } = conf

--------------------------------------------------------------------------------
-- Miscellaneous utilities:
templateInstr :: Term -> Maybe TemplateInstr
templateInstr term = case While.readInstr term of
    Just (While.ISet var expr) -> Just $ TISet var expr
    _                          -> Nothing

-- Reads a template from the ASP at the given file path.
readTemplate :: FilePath -> IO Template
readTemplate path = do
    let options = runClingoOptions{ rcEchoStdout=False, rcEchoInput=False }
    runClingo options [CIFile path] >>= \result -> case result of
        CRSatisfiable (answer : _) ->
            either error return $ answerToTemplate answer
        _ ->
            error "unsatisfiable"

-- Print a template in a human-readable form:
printTemplate :: Template -> IO ()
printTemplate = mapM_ putStrLn . showTemplate

showTemplate :: Template -> [String]
showTemplate Template{ teParts=parts, teConf=conf }
  = ["[Program vars: "++pvsString++"; Logic vars: "++lvsString++"]"]    
    ++ zipWith numLine [1..] (concatMap showTemplatePart parts)
  where
    pvsString = vsString (tcProgramVars conf)
    lvsString = vsString (tcLogicVars conf)

    vsString :: [Variable] -> String
    vsString [] = "none"
    vsString vs = unwords . sort $ vs

    numLine :: Integer -> String -> String
    numLine n l
      = (++ l) . reverse . take 6 $ (reverse $ show n ++ ". ") ++ repeat ' '

showTemplatePart :: TePart -> [String]
showTemplatePart = showTemplatePart' ""

showTemplatePart' :: String -> TePart -> [String]
showTemplatePart' ind part = case part of
    TPCond CTPre  con              -> [ind ++ "pre: "++ showCondition con ++"."]
    TPCond CTMid  con              -> [ind ++ "mid: "++ showCondition con ++"."]
    TPCond CTPost con              -> [ind ++ "post: "++ showCondition con ++"."]
    TPInv (Invariant con)          -> [ind ++ "inv: "++ showCondition con ++"."]
    TPVar variant                  -> [ind ++ "var: "++ showVariant variant ++"."]
    TPInstr (TISet (Name var) exp) -> [ind ++ var ++" = "++ While.showExpr exp]
    TPBlock BTIf guard body ->
        [ind ++ "if (" ++ While.showGuard guard ++ "):"]
        ++ (showTemplatePart' ("    " ++ ind) =<< body) ++
        [ind ++ "end_if"]
    TPBlock BTWhile guard body ->
        [ind ++ "while (" ++ While.showGuard guard ++ "):"]
        ++ (showTemplatePart' ("    " ++ ind) =<< body) ++
        [ind ++ "end_while"]

-- Showing other data types:
showCondition :: Condition -> String
showCondition ""   = "(none)"
showCondition cond = cond

showVariant :: Variant -> String
showVariant (VarVariant var con dir)
  = var ++"; "++ con ++"; "++ showVariantDirection dir
showVariant (ExpVariant exp dir)
  = exp ++"; " ++ showVariantDirection dir

showVariantDirection :: VariantDirection -> String
showVariantDirection VDIncreasing = "increasing"
showVariantDirection VDDecreasing = "decreasing"

blockEnd :: BlockType -> Name
blockEnd BTIf    = "end_if"
blockEnd BTWhile = "end_while"

-- Reading data types:
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
