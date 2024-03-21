module Utilities where
import Texts
import Blocks
import PGF

-- Important small functions
extract :: Maybe a -> a
extract (Just x) = x

-- Replacing metavariables with the corresponding integers
changeMetaToInt :: Expr -> Expr
changeMetaToInt e =    case unMeta e of
                        Just i -> mkInt i
                        Nothing ->
                                case unApp e of
                                    Just (f, args) -> mkApp f (fmap changeMetaToInt args)
                                    Nothing -> e


--- Utilities for simplifications

extractNamesFromPCNoun :: GprimClassNoun -> Gnames
extractNamesFromPCNoun (GrN0ToPcNoun _ x) = x

extractRawNoun0FromPCNoun :: GprimClassNoun -> Texts.GrawNoun0
extractRawNoun0FromPCNoun (GrN0ToPcNoun x _) = x

extractNamesFromNotion :: Gnotion -> Gnames
extractNamesFromNotion (GprClNounToNotion x) = extractNamesFromPCNoun x
extractNamesFromNotion (GprClNounRAttrToNotion x _ ) = extractNamesFromPCNoun x
extractNamesFromNotion (GprLAttrClNounToNotion _ x) = extractNamesFromPCNoun x
extractNamesFromNotion (GprLAttrClNounRAttrToNotion _ x _) = extractNamesFromPCNoun x

extractNamesFromQNotion :: GquantifiedNotion -> Gnames
extractNamesFromQNotion (GallNotion x) = extractNamesFromNotion x
extractNamesFromQNotion (GsomeNotion x) = extractNamesFromNotion x
extractNamesFromQNotion (GnoNotion x) = extractNamesFromNotion x

extractVarFromName :: Gnames -> Texts.Gvariable
extractVarFromName (GlistVarToName x) = x

extractVarFromQNotion :: GquantifiedNotion -> Texts.Gvariable
extractVarFromQNotion = extractVarFromName . extractNamesFromQNotion

extractVarFromPCNoun :: GprimClassNoun -> Texts.Gvariable
extractVarFromPCNoun = extractVarFromName . extractNamesFromPCNoun

extractVarFromNotion :: Gnotion -> Texts.Gvariable
extractVarFromNotion = extractVarFromName . extractNamesFromNotion

putNameInPCNoun :: Gnames -> GprimClassNoun -> GprimClassNoun
putNameInPCNoun name (GrN0ToPcNoun y _) = GrN0ToPcNoun y name

putNameInNotion :: Gnames -> Gnotion -> Gnotion
putNameInNotion name (GprClNounToNotion prc) = GprClNounToNotion (putNameInPCNoun name prc)
putNameInNotion name (GprClNounRAttrToNotion prc rattr ) = GprClNounRAttrToNotion (putNameInPCNoun name prc) rattr
putNameInNotion name (GprLAttrClNounToNotion lattr prc) = GprLAttrClNounToNotion lattr (putNameInPCNoun name prc)
putNameInNotion name (GprLAttrClNounRAttrToNotion lattr prc rattr) = GprLAttrClNounRAttrToNotion lattr (putNameInPCNoun name prc) rattr

putNameInQNotion :: Gnames -> GquantifiedNotion -> GquantifiedNotion
putNameInQNotion name (GallNotion x) = GallNotion (putNameInNotion name x)
putNameInQNotion name (GsomeNotion x) = GsomeNotion (putNameInNotion name x)
putNameInQNotion name (GnoNotion x) = GnoNotion (putNameInNotion name x)

extractPrAdjFromSimAdj :: GprimSimpleAdjective -> GprimAdjective
extractPrAdjFromSimAdj (GrA0ToPSAdj x) = GrA0ToPAdj x

mkStmFromLAttr :: Texts.Gvariable -> GleftAttribute -> Gstatement
mkStmFromLAttr x (GprSimpAdjToLAttrib psa) = GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisPredToDPred (GprimAdjToIsPred Gpos (extractPrAdjFromSimAdj psa)))

mkStmFromRAttr :: Texts.Gvariable -> GrightAttribute -> Gstatement
mkStmFromRAttr x (GisPrToRAttr ispred) = GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisPredToDPred ispred)
mkStmFromRAttr x (GdoesPrToRAttr dpred) = GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) dpred
mkStmFromRAttr _ (GstmToRAttr stm) = stm



-- helper function that takes a stm of the form x is an integer such that X and spits out x is an integer and the X :: statement.
-- 1. x is an integer such that X -> x is an integer :: Statement
extractHeadStatement :: Gstatement -> Gstatement
extractHeadStatement (GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisAPredToDPred (GclNounToIs_aPred pol (GprClNounRAttrToNotion pcn _)))) = GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisAPredToDPred (GclNounToIs_aPred pol (GprClNounToNotion pcn)))
-- 2. x is an integer such that X -> x :: Statement
extractRAStatement :: Gstatement -> Gstatement
extractRAStatement (GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisAPredToDPred (GclNounToIs_aPred pol (GprClNounRAttrToNotion pcn (GstmToRAttr stm))))) = stm


----- Utilities for translations
checkPolarity :: Gpolarity -> (Gproposition -> Gproposition)
checkPolarity Gneg = Gnot
checkPolarity _ = id

-- Utilities for unification
putNameInDoesPred :: Texts.Gnames -> GdoesPredicate -> GdoesPredicate
putNameInDoesPred x (GisAPredToDPred (GclNounToIs_aPred pol not)) = GisAPredToDPred (GclNounToIs_aPred pol (putNameInNotion x not))
putNameInDoesPred _ y = y 

