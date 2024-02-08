module Translations where

import PGF
import Texts -- English file
import Blocks -- Lean grammar
import Utilities


-- The integers (fully done)
translateInt :: Texts.GInt -> Blocks.GInt
translateInt (Texts.GInt x) = Blocks.GInt x

-- Translating lexicon (fully done)
translateVariable :: Texts.Gvariable -> Blocks.Gvariable
translateVariable Texts.GA_VAR = Blocks.GA_VAR
translateVariable Texts.GB_VAR = Blocks.GB_VAR
translateVariable Texts.GC_VAR = Blocks.GC_VAR
translateVariable Texts.GK_VAR = Blocks.GK_VAR
translateVariable Texts.GN_VAR = Blocks.GN_VAR
translateVariable Texts.GR_VAR = Blocks.GR_VAR
translateVariable Texts.GX_VAR = Blocks.GX_VAR
translateVariable Texts.GY_VAR = Blocks.GY_VAR
translateVariable Texts.GZ_VAR = Blocks.GZ_VAR
translateVariable (GknownName (Texts.GInt x)) = GintToVar (Blocks.GInt x)

translateRawNoun0 :: Texts.GrawNoun0 -> Blocks.GrawNoun0
translateRawNoun0 Texts.GREAL_NUMBER = Blocks.GREAL_NUMBER
translateRawNoun0 Texts.GINTEGER = Blocks.GINTEGER
translateRawNoun0 Texts.GRATIONAL = Blocks.GRATIONAL

translateRawNoun0ToType :: Texts.GrawNoun0 -> Blocks.Gtype
translateRawNoun0ToType x = GrN0ToType $ translateRawNoun0 x

translateRawNoun2 :: Texts.GrawNoun2 -> Blocks.GrawNoun2
translateRawNoun2 Texts.GEXP = Blocks.GEXP
translateRawNoun2 Texts.GSUM = Blocks.GSUM
translateRawNoun2 Texts.GMINUS = Blocks.GMINUS
translateRawNoun2 Texts.GPRODUCT = Blocks.GPRODUCT
translateRawNoun2 Texts.GFRAC = Blocks.GFRAC

translateRawNoun2ToFun2 :: Texts.GrawNoun2 -> Blocks.Gfun2
translateRawNoun2ToFun2 x = GrN2ToFun2 $ translateRawNoun2 x

translateRawAdjective0 :: Texts.GrawAdjective0 -> Blocks.GrawAdjective0
translateRawAdjective0 Texts.GPOSITIVE = Blocks.GPOSITIVE
translateRawAdjective0 Texts.GODD = Blocks.GODD
translateRawAdjective0 Texts.GEVEN = Blocks.GEVEN
translateRawAdjective0 Texts.GNONNEGATIVE = Blocks.GNONNEGATIVE

translateRawAdjective0ToRel1 :: Texts.GrawAdjective0 -> Blocks.Grel1
translateRawAdjective0ToRel1 x = GrA0ToRel1 $ translateRawAdjective0 x

translateRawAdjective1 :: Texts.GrawAdjective1 -> Blocks.GrawAdjective1
translateRawAdjective1 Texts.GLESS_THAN = Blocks.GLESS_THAN
translateRawAdjective1 Texts.GLESS_TE = Blocks.GLESS_TE
translateRawAdjective1 Texts.GGREATER_THAN = Blocks.GGREATER_THAN
translateRawAdjective1 Texts.GGREATER_TE = Blocks.GGREATER_TE
translateRawAdjective1 Texts.GNOT_EQUAL = Blocks.GNOT_EQUAL
translateRawAdjective1 Texts.GEQUAL = Blocks.GEQUAL

translateRawAdjective1ToRel2 :: Texts.GrawAdjective1 -> Blocks.Grel2
translateRawAdjective1ToRel2 x = GrA1ToRel2 $ translateRawAdjective1 x

-- Translating Names to the corresponding variable (fully done)
translateNames :: Gnames -> Blocks.Gvariable
translateNames (GlistVarToName var) = translateVariable var


-- PrimDefiniteNouns become entities
translatePrDefNoun :: GprimDefiniteNoun -> Gentity
translatePrDefNoun (GrN2ToPDNoun rn2 t1 t2) = Gfun2ToEntity (translateRawNoun2ToFun2 rn2) (translateTerm t1) (translateTerm t2)

-- Definite terms become entities
translateDefTerm :: GdefiniteTerm -> Gentity
translateDefTerm (GvarToDefTerm var) = GvarToEntity (translateVariable var)
translateDefTerm (GintToDefTerm int) = GintToEntity (translateInt int)
translateDefTerm (GprDefNounToDefTerm pdn) = translatePrDefNoun pdn

-- All terms which are definite terms become entities (other terms are quantified notions which are not entities)
translateTerm :: Gterm -> Gentity
translateTerm (GdefTermToTerm dt) = translateDefTerm dt

---- Text level translation (fully done)
translateText :: Texts.Gtext -> Blocks.Gblock
translateText (GthmToText ex) = GexmBltoBl $ translateExample ex

-- Example level translation (fully done)
translateExample :: Texts.Gexample -> Blocks.GexampleBl
translateExample (GassToExm lass stm) = GassToExmBl (translateLAssumption lass) (translateStatement stm)

-- List of Assumptions translation (fully done)
translateLAssumption :: Texts.GLassumption -> Blocks.GLassumption
translateLAssumption Texts.GBassumption = Blocks.GBassumption
translateLAssumption (Texts.GCassumption ass lass) = Blocks.GCassumption (translateAssumption ass) (translateLAssumption lass)

-- Assumptions translation (Use pattern matching to figure out whether its a type defn or a hypothesis defn)
translateAssumption :: Texts.Gassumption -> Blocks.Gassumption
translateAssumption (GstmToAssumption (GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm x)) (GisAPredToDPred (GclNounToIs_aPred Gpos (GprClNounToNotion (GrN0ToPcNoun rn _)))))) = GtypeDefToAssump (translateVariable x) (translateRawNoun0ToType rn)
translateAssumption (GstmToAssumption stm) = GpropToAssump GH (translateStatement stm)

-- Helper function to translate primAdjective
mkPropFromPrimAdj :: Texts.Gterm -> GprimAdjective -> Gproposition
mkPropFromPrimAdj x (GrA0ToPAdj radj0) = Grel1ToProp (translateRawAdjective0ToRel1 radj0) (translateTerm x)
mkPropFromPrimAdj x (GrA1ToPAdj radj1 t) = Grel2ToProp (translateRawAdjective1ToRel2 radj1) (translateTerm x) (translateTerm t)

-- The main part of translation is translating statements
translateStatement :: Gstatement -> Gproposition
translateStatement (GandStm stm1 stm2) = Gand (translateStatement stm1) (translateStatement stm2)
translateStatement (GorStm stm1 stm2) = Gor (translateStatement stm1) (translateStatement stm2)
translateStatement (GifThenStm stm1 stm2) = Gimplies (translateStatement stm1) (translateStatement stm2)
translateStatement (GiffStm stm1 stm2) = Giff (translateStatement stm1) (translateStatement stm2)
translateStatement (GnotStm stm) = Gnot (translateStatement stm)  -- Basic logical operations till here

-- statements of the form TERM DOESPREDICATE
translateStatement (GtermDoesPredToStm t (GisPredToDPred (GprimAdjToIsPred pol padj))) = checkPolarity pol $ mkPropFromPrimAdj t padj

-- of the form there exists a NOTION such that stm
translateStatement (GnotionsToStm (GprClNounRAttrToNotion pcn (GstmToRAttr stm))) = Gexists (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement stm)
-- of the form there exists no NOTION such that stm
translateStatement (GnotionNoToStm (GprClNounRAttrToNotion pcn (GstmToRAttr stm))) = GforAll (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement (GnotStm stm))

-- of the form for all notion stm2 
translateStatement (GqNotStmToStm (GallNotion (GprClNounToNotion pcn)) stm2) = GforAll (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement stm2)
-- of the form for all notion such that stm1, stm2
translateStatement (GqNotStmToStm (GallNotion (GprClNounRAttrToNotion pcn (GstmToRAttr stm1))) stm2) = GforAll (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement (GifThenStm stm1 stm2))

-- of the form for some notion stm2
translateStatement (GqNotStmToStm (GsomeNotion (GprClNounToNotion pcn)) stm2) = Gexists (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement stm2)
-- of the form for some notion such that stm1, stm2
translateStatement (GqNotStmToStm (GsomeNotion (GprClNounRAttrToNotion pcn (GstmToRAttr stm1))) stm2) = Gexists (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement (GandStm stm1 stm2))

-- of the form for no notion, stm2
translateStatement (GqNotStmToStm (GnoNotion (GprClNounToNotion pcn)) stm2) = GforAll (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement $ GnotStm stm2)
-- of the form for npo notion such that stm1, stm2
translateStatement (GqNotStmToStm (GnoNotion (GprClNounRAttrToNotion pcn (GstmToRAttr stm1))) stm2) = GforAll (translateNames $ extractNamesFromPCNoun pcn) (translateRawNoun0ToType $ extractRawNoun0FromPCNoun pcn) (translateStatement (GifThenStm stm1 (GnotStm stm2)))

