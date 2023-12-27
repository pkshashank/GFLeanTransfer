module Simplifications where

import PGF
import Texts -- Eng PGF
import Utilities


simplifyText :: Gtext -> Gtext
simplifyText (GthmToText x) = GthmToText $ simplifyExample x

simplifyExample :: Gexample -> Gexample
simplifyExample (GassToExm lass stm) = GassToExm (simplifyLassumption lass) (simplifyStatement stm)

simplifyLassumption :: GLassumption -> GLassumption
-- Simplifying the list of assumptions. Chains of `and` become seperate assumptions.
-- Assume A and B --> Assume A. Assume B.
simplifyLassumption (GCassumption (GstmToAssumption (GandStm stm1 stm2)) lass) = GCassumption (GstmToAssumption stm1) (GCassumption (GstmToAssumption stm2) (simplifyLassumption lass))
simplifyLassumption (GCassumption ass lass) = GCassumption (simplifyAssumption ass) (simplifyLassumption lass)
simplifyLassumption x = x

simplifyAssumption :: Gassumption-> Gassumption
simplifyAssumption (GstmToAssumption x) = GstmToAssumption (simplifyStatement x)


simplifyStatement :: Gstatement -> Gstatement
simplifyStatement (GandStm x1 x2) = GandStm (simplifyStatement x1) (simplifyStatement x2)
simplifyStatement (GorStm x1 x2) = GorStm (simplifyStatement x1) (simplifyStatement x2)
simplifyStatement (GifThenStm x1 x2) = GifThenStm (simplifyStatement x1) (simplifyStatement x2)
simplifyStatement (GiffStm x1 x2) = GiffStm (simplifyStatement x1) (simplifyStatement x2)
simplifyStatement (GnotStm x) = GnotStm (simplifyStatement x)
simplifyStatement (GqNotStmToStm qn stm) = GqNotStmToStm (simplifyQNotion qn) (simplifyStatement stm)

simplifyStatement (GtermDoesPredToStm (GqNotionToTerm qn) dp) = GqNotStmToStm (simplifyQNotion qn) (GtermDoesPredToStm (GdefTermToTerm (GvarToDefTerm (extractVarFromQNotion qn))) (simplifyDoesPred (putNameInDoesPred (extractNamesFromQNotion qn) dp))) -- Unification with quantified notion
simplifyStatement (GtermDoesPredToStm trm@(GdefTermToTerm (GvarToDefTerm x)) dp) = GtermDoesPredToStm (simplifyTerm trm) (simplifyDoesPred (putNameInDoesPred (GlistVarToName x) dp)) -- Unification with a variable name
simplifyStatement (GtermDoesPredToStm trm dp) = GtermDoesPredToStm (simplifyTerm trm) (simplifyDoesPred dp) -- all the other cases

simplifyStatement (GnotionsToStm not) = GnotionsToStm (simplifyNotion not)
simplifyStatement (GnotionNoToStm not) = GnotionNoToStm (simplifyNotion not)


-- every [NOTION] [NAME] is ADJECTIVE --> for every [NOTION] [NAME], [NAME] is ADJECTIVE

simplifyQNotion :: GquantifiedNotion -> GquantifiedNotion
simplifyQNotion (GallNotion x) = GallNotion (simplifyNotion x)
simplifyQNotion (GsomeNotion x) = GsomeNotion (simplifyNotion x)
simplifyQNotion (GnoNotion x) = GnoNotion (simplifyNotion x)


simplifyNotion :: Gnotion -> Gnotion
simplifyNotion (GprClNounRAttrToNotion prc rattr) = GprClNounRAttrToNotion prc (GstmToRAttr (mkStmFromRAttr (extractVarFromPCNoun prc) rattr))
simplifyNotion (GprLAttrClNounToNotion lattr prc) = GprClNounRAttrToNotion prc (GstmToRAttr (mkStmFromLAttr (extractVarFromPCNoun prc) lattr))
simplifyNotion (GprLAttrClNounRAttrToNotion lattr prc rattr) = GprClNounRAttrToNotion prc (GstmToRAttr (GandStm (mkStmFromLAttr (extractVarFromPCNoun prc) lattr) (mkStmFromRAttr (extractVarFromPCNoun prc) rattr)))
simplifyNotion x = x 

simplifyTerm :: Gterm -> Gterm
simplifyTerm (GqNotionToTerm qn) = GqNotionToTerm (simplifyQNotion qn)
simplifyTerm (GdefTermToTerm dt) = GdefTermToTerm (simplifyDefTerm dt)

simplifyDefTerm :: GdefiniteTerm -> GdefiniteTerm
simplifyDefTerm (GprDefNounToDefTerm pdn) = GprDefNounToDefTerm (simplifyPrDefNoun pdn)
simplifyDefTerm x = x 

simplifyPrDefNoun :: GprimDefiniteNoun -> GprimDefiniteNoun
simplifyPrDefNoun (GrN1ToPDNoun r t) = GrN1ToPDNoun r (simplifyTerm t)
simplifyPrDefNoun (GrN2ToPDNoun r t1 t2) = GrN2ToPDNoun r (simplifyTerm t1) (simplifyTerm t2)

simplifyPrAdjective :: GprimAdjective -> GprimAdjective
simplifyPrAdjective (GrA1ToPAdj r t) = GrA1ToPAdj r (simplifyTerm t)
simplifyPrAdjective (GrA2ToPAdj r t1 t2) = GrA2ToPAdj r (simplifyTerm t1) (simplifyTerm t2)
simplifyPrAdjective x = x 

simplifyDoesPred :: GdoesPredicate -> GdoesPredicate
simplifyDoesPred (GisPredToDPred ispred) = GisPredToDPred (simplifyIsPred ispred)
simplifyDoesPred (GisAPredToDPred isapred) = GisAPredToDPred (simplifyIsAPred isapred)
simplifyDoesPred x = x 

simplifyIsPred :: GisPredicate -> GisPredicate
simplifyIsPred (GprimAdjToIsPred pol pradj) = GprimAdjToIsPred pol (simplifyPrAdjective pradj)

simplifyIsAPred :: Gis_aPredicate -> Gis_aPredicate
simplifyIsAPred (GclNounToIs_aPred pol not) = GclNounToIs_aPred pol (simplifyNotion not)
simplifyIsAPred (GdeftrmToIs_aPred pol dterm) = GdeftrmToIs_aPred pol (simplifyDefTerm dterm)