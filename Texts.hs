module Texts where

import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GLassumption =
   GBassumption 
 | GCassumption Gassumption GLassumption 
  deriving Show

data Gassumption = GstmToAssumption Gstatement 
  deriving Show

data GdefiniteTerm =
   GintToDefTerm GInt 
 | GprDefNounToDefTerm GprimDefiniteNoun 
 | GvarToDefTerm Gvariable 
  deriving Show

data GdoesPredicate =
   GisAPredToDPred Gis_aPredicate 
 | GisPredToDPred GisPredicate 
  deriving Show

data Gexample = GassToExm GLassumption Gstatement 
  deriving Show

data GisPredicate = GprimAdjToIsPred Gpolarity GprimAdjective 
  deriving Show

data Gis_aPredicate =
   GclNounToIs_aPred Gpolarity Gnotion 
 | GdeftrmToIs_aPred Gpolarity GdefiniteTerm 
  deriving Show

data GleftAttribute = GprSimpAdjToLAttrib GprimSimpleAdjective 
  deriving Show

data Gnames = GlistVarToName Gvariable 
  deriving Show

data Gnotion =
   GprClNounRAttrToNotion GprimClassNoun GrightAttribute 
 | GprClNounToNotion GprimClassNoun 
 | GprLAttrClNounRAttrToNotion GleftAttribute GprimClassNoun GrightAttribute 
 | GprLAttrClNounToNotion GleftAttribute GprimClassNoun 
  deriving Show

data Gpolarity =
   Gneg 
 | Gpos 
  deriving Show

data GprimAdjective =
   GrA0ToPAdj GrawAdjective0 
 | GrA1ToPAdj GrawAdjective1 Gterm 
  deriving Show

data GprimClassNoun = GrN0ToPcNoun GrawNoun0 Gnames 
  deriving Show

data GprimDefiniteNoun = GrN2ToPDNoun GrawNoun2 Gterm Gterm 
  deriving Show

data GprimSimpleAdjective = GrA0ToPSAdj GrawAdjective0 
  deriving Show

data GquantifiedNotion =
   GallNotion Gnotion 
 | GnoNotion Gnotion 
 | GsomeNotion Gnotion 
  deriving Show

data GrawAdjective0 =
   GEVEN 
 | GNEGATIVE 
 | GNONNEGATIVE 
 | GODD 
 | GPOSITIVE 
  deriving Show

data GrawAdjective1 =
   GEQUAL 
 | GGREATER_TE 
 | GGREATER_THAN 
 | GLESS_TE 
 | GLESS_THAN 
 | GNOT_EQUAL 
  deriving Show

data GrawNoun0 =
   GINTEGER 
 | GRATIONAL 
 | GREAL_NUMBER 
  deriving Show

data GrawNoun2 =
   GEXP 
 | GFRAC 
 | GMINUS 
 | GPRODUCT 
 | GSUM 
  deriving Show

data GrightAttribute =
   GdoesPrToRAttr GdoesPredicate 
 | GisPrToRAttr GisPredicate 
 | GstmToRAttr Gstatement 
  deriving Show

data Gstatement =
   GandStm Gstatement Gstatement 
 | GifThenStm Gstatement Gstatement 
 | GiffStm Gstatement Gstatement 
 | GnotStm Gstatement 
 | GnotionNoToStm Gnotion 
 | GnotionsToStm Gnotion 
 | GorStm Gstatement Gstatement 
 | GqNotStmToStm GquantifiedNotion Gstatement 
 | GtermDoesPredToStm Gterm GdoesPredicate 
  deriving Show

data Gterm =
   GdefTermToTerm GdefiniteTerm 
 | GqNotionToTerm GquantifiedNotion 
  deriving Show

data Gtext = GthmToText Gexample 
  deriving Show

data Gvariable =
   GA_VAR 
 | GB_VAR 
 | GC_VAR 
 | GK_VAR 
 | GM_VAR 
 | GN_VAR 
 | GR_VAR 
 | GX_VAR 
 | GY_VAR 
 | GZ_VAR 
 | GknownName GInt 
  deriving Show


instance Gf GLassumption where
  gf GBassumption = mkApp (mkCId "Bassumption") []
  gf (GCassumption x1 x2) = mkApp (mkCId "Cassumption") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Bassumption" -> GBassumption 
      Just (i,[x1,x2]) | i == mkCId "Cassumption" -> GCassumption (fg x1) (fg x2)


      _ -> error ("no Lassumption " ++ show t)

instance Gf Gassumption where
  gf (GstmToAssumption x1) = mkApp (mkCId "stmToAssumption") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "stmToAssumption" -> GstmToAssumption (fg x1)


      _ -> error ("no assumption " ++ show t)

instance Gf GdefiniteTerm where
  gf (GintToDefTerm x1) = mkApp (mkCId "intToDefTerm") [gf x1]
  gf (GprDefNounToDefTerm x1) = mkApp (mkCId "prDefNounToDefTerm") [gf x1]
  gf (GvarToDefTerm x1) = mkApp (mkCId "varToDefTerm") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "intToDefTerm" -> GintToDefTerm (fg x1)
      Just (i,[x1]) | i == mkCId "prDefNounToDefTerm" -> GprDefNounToDefTerm (fg x1)
      Just (i,[x1]) | i == mkCId "varToDefTerm" -> GvarToDefTerm (fg x1)


      _ -> error ("no definiteTerm " ++ show t)

instance Gf GdoesPredicate where
  gf (GisAPredToDPred x1) = mkApp (mkCId "isAPredToDPred") [gf x1]
  gf (GisPredToDPred x1) = mkApp (mkCId "isPredToDPred") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "isAPredToDPred" -> GisAPredToDPred (fg x1)
      Just (i,[x1]) | i == mkCId "isPredToDPred" -> GisPredToDPred (fg x1)


      _ -> error ("no doesPredicate " ++ show t)

instance Gf Gexample where
  gf (GassToExm x1 x2) = mkApp (mkCId "assToExm") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "assToExm" -> GassToExm (fg x1) (fg x2)


      _ -> error ("no example " ++ show t)

instance Gf GisPredicate where
  gf (GprimAdjToIsPred x1 x2) = mkApp (mkCId "primAdjToIsPred") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "primAdjToIsPred" -> GprimAdjToIsPred (fg x1) (fg x2)


      _ -> error ("no isPredicate " ++ show t)

instance Gf Gis_aPredicate where
  gf (GclNounToIs_aPred x1 x2) = mkApp (mkCId "clNounToIs_aPred") [gf x1, gf x2]
  gf (GdeftrmToIs_aPred x1 x2) = mkApp (mkCId "deftrmToIs_aPred") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "clNounToIs_aPred" -> GclNounToIs_aPred (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "deftrmToIs_aPred" -> GdeftrmToIs_aPred (fg x1) (fg x2)


      _ -> error ("no is_aPredicate " ++ show t)

instance Gf GleftAttribute where
  gf (GprSimpAdjToLAttrib x1) = mkApp (mkCId "prSimpAdjToLAttrib") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "prSimpAdjToLAttrib" -> GprSimpAdjToLAttrib (fg x1)


      _ -> error ("no leftAttribute " ++ show t)

instance Gf Gnames where
  gf (GlistVarToName x1) = mkApp (mkCId "listVarToName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "listVarToName" -> GlistVarToName (fg x1)


      _ -> error ("no names " ++ show t)

instance Gf Gnotion where
  gf (GprClNounRAttrToNotion x1 x2) = mkApp (mkCId "prClNounRAttrToNotion") [gf x1, gf x2]
  gf (GprClNounToNotion x1) = mkApp (mkCId "prClNounToNotion") [gf x1]
  gf (GprLAttrClNounRAttrToNotion x1 x2 x3) = mkApp (mkCId "prLAttrClNounRAttrToNotion") [gf x1, gf x2, gf x3]
  gf (GprLAttrClNounToNotion x1 x2) = mkApp (mkCId "prLAttrClNounToNotion") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "prClNounRAttrToNotion" -> GprClNounRAttrToNotion (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "prClNounToNotion" -> GprClNounToNotion (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "prLAttrClNounRAttrToNotion" -> GprLAttrClNounRAttrToNotion (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "prLAttrClNounToNotion" -> GprLAttrClNounToNotion (fg x1) (fg x2)


      _ -> error ("no notion " ++ show t)

instance Gf Gpolarity where
  gf Gneg = mkApp (mkCId "neg") []
  gf Gpos = mkApp (mkCId "pos") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "neg" -> Gneg 
      Just (i,[]) | i == mkCId "pos" -> Gpos 


      _ -> error ("no polarity " ++ show t)

instance Gf GprimAdjective where
  gf (GrA0ToPAdj x1) = mkApp (mkCId "rA0ToPAdj") [gf x1]
  gf (GrA1ToPAdj x1 x2) = mkApp (mkCId "rA1ToPAdj") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rA0ToPAdj" -> GrA0ToPAdj (fg x1)
      Just (i,[x1,x2]) | i == mkCId "rA1ToPAdj" -> GrA1ToPAdj (fg x1) (fg x2)


      _ -> error ("no primAdjective " ++ show t)

instance Gf GprimClassNoun where
  gf (GrN0ToPcNoun x1 x2) = mkApp (mkCId "rN0ToPcNoun") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "rN0ToPcNoun" -> GrN0ToPcNoun (fg x1) (fg x2)


      _ -> error ("no primClassNoun " ++ show t)

instance Gf GprimDefiniteNoun where
  gf (GrN2ToPDNoun x1 x2 x3) = mkApp (mkCId "rN2ToPDNoun") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "rN2ToPDNoun" -> GrN2ToPDNoun (fg x1) (fg x2) (fg x3)


      _ -> error ("no primDefiniteNoun " ++ show t)

instance Gf GprimSimpleAdjective where
  gf (GrA0ToPSAdj x1) = mkApp (mkCId "rA0ToPSAdj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rA0ToPSAdj" -> GrA0ToPSAdj (fg x1)


      _ -> error ("no primSimpleAdjective " ++ show t)

instance Gf GquantifiedNotion where
  gf (GallNotion x1) = mkApp (mkCId "allNotion") [gf x1]
  gf (GnoNotion x1) = mkApp (mkCId "noNotion") [gf x1]
  gf (GsomeNotion x1) = mkApp (mkCId "someNotion") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "allNotion" -> GallNotion (fg x1)
      Just (i,[x1]) | i == mkCId "noNotion" -> GnoNotion (fg x1)
      Just (i,[x1]) | i == mkCId "someNotion" -> GsomeNotion (fg x1)


      _ -> error ("no quantifiedNotion " ++ show t)

instance Gf GrawAdjective0 where
  gf GEVEN = mkApp (mkCId "EVEN") []
  gf GNEGATIVE = mkApp (mkCId "NEGATIVE") []
  gf GNONNEGATIVE = mkApp (mkCId "NONNEGATIVE") []
  gf GODD = mkApp (mkCId "ODD") []
  gf GPOSITIVE = mkApp (mkCId "POSITIVE") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EVEN" -> GEVEN 
      Just (i,[]) | i == mkCId "NEGATIVE" -> GNEGATIVE 
      Just (i,[]) | i == mkCId "NONNEGATIVE" -> GNONNEGATIVE 
      Just (i,[]) | i == mkCId "ODD" -> GODD 
      Just (i,[]) | i == mkCId "POSITIVE" -> GPOSITIVE 


      _ -> error ("no rawAdjective0 " ++ show t)

instance Gf GrawAdjective1 where
  gf GEQUAL = mkApp (mkCId "EQUAL") []
  gf GGREATER_TE = mkApp (mkCId "GREATER_TE") []
  gf GGREATER_THAN = mkApp (mkCId "GREATER_THAN") []
  gf GLESS_TE = mkApp (mkCId "LESS_TE") []
  gf GLESS_THAN = mkApp (mkCId "LESS_THAN") []
  gf GNOT_EQUAL = mkApp (mkCId "NOT_EQUAL") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EQUAL" -> GEQUAL 
      Just (i,[]) | i == mkCId "GREATER_TE" -> GGREATER_TE 
      Just (i,[]) | i == mkCId "GREATER_THAN" -> GGREATER_THAN 
      Just (i,[]) | i == mkCId "LESS_TE" -> GLESS_TE 
      Just (i,[]) | i == mkCId "LESS_THAN" -> GLESS_THAN 
      Just (i,[]) | i == mkCId "NOT_EQUAL" -> GNOT_EQUAL 


      _ -> error ("no rawAdjective1 " ++ show t)

instance Gf GrawNoun0 where
  gf GINTEGER = mkApp (mkCId "INTEGER") []
  gf GRATIONAL = mkApp (mkCId "RATIONAL") []
  gf GREAL_NUMBER = mkApp (mkCId "REAL_NUMBER") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "INTEGER" -> GINTEGER 
      Just (i,[]) | i == mkCId "RATIONAL" -> GRATIONAL 
      Just (i,[]) | i == mkCId "REAL_NUMBER" -> GREAL_NUMBER 


      _ -> error ("no rawNoun0 " ++ show t)

instance Gf GrawNoun2 where
  gf GEXP = mkApp (mkCId "EXP") []
  gf GFRAC = mkApp (mkCId "FRAC") []
  gf GMINUS = mkApp (mkCId "MINUS") []
  gf GPRODUCT = mkApp (mkCId "PRODUCT") []
  gf GSUM = mkApp (mkCId "SUM") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EXP" -> GEXP 
      Just (i,[]) | i == mkCId "FRAC" -> GFRAC 
      Just (i,[]) | i == mkCId "MINUS" -> GMINUS 
      Just (i,[]) | i == mkCId "PRODUCT" -> GPRODUCT 
      Just (i,[]) | i == mkCId "SUM" -> GSUM 


      _ -> error ("no rawNoun2 " ++ show t)

instance Gf GrightAttribute where
  gf (GdoesPrToRAttr x1) = mkApp (mkCId "doesPrToRAttr") [gf x1]
  gf (GisPrToRAttr x1) = mkApp (mkCId "isPrToRAttr") [gf x1]
  gf (GstmToRAttr x1) = mkApp (mkCId "stmToRAttr") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "doesPrToRAttr" -> GdoesPrToRAttr (fg x1)
      Just (i,[x1]) | i == mkCId "isPrToRAttr" -> GisPrToRAttr (fg x1)
      Just (i,[x1]) | i == mkCId "stmToRAttr" -> GstmToRAttr (fg x1)


      _ -> error ("no rightAttribute " ++ show t)

instance Gf Gstatement where
  gf (GandStm x1 x2) = mkApp (mkCId "andStm") [gf x1, gf x2]
  gf (GifThenStm x1 x2) = mkApp (mkCId "ifThenStm") [gf x1, gf x2]
  gf (GiffStm x1 x2) = mkApp (mkCId "iffStm") [gf x1, gf x2]
  gf (GnotStm x1) = mkApp (mkCId "notStm") [gf x1]
  gf (GnotionNoToStm x1) = mkApp (mkCId "notionNoToStm") [gf x1]
  gf (GnotionsToStm x1) = mkApp (mkCId "notionsToStm") [gf x1]
  gf (GorStm x1 x2) = mkApp (mkCId "orStm") [gf x1, gf x2]
  gf (GqNotStmToStm x1 x2) = mkApp (mkCId "qNotStmToStm") [gf x1, gf x2]
  gf (GtermDoesPredToStm x1 x2) = mkApp (mkCId "termDoesPredToStm") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "andStm" -> GandStm (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ifThenStm" -> GifThenStm (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "iffStm" -> GiffStm (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "notStm" -> GnotStm (fg x1)
      Just (i,[x1]) | i == mkCId "notionNoToStm" -> GnotionNoToStm (fg x1)
      Just (i,[x1]) | i == mkCId "notionsToStm" -> GnotionsToStm (fg x1)
      Just (i,[x1,x2]) | i == mkCId "orStm" -> GorStm (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "qNotStmToStm" -> GqNotStmToStm (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "termDoesPredToStm" -> GtermDoesPredToStm (fg x1) (fg x2)


      _ -> error ("no statement " ++ show t)

instance Gf Gterm where
  gf (GdefTermToTerm x1) = mkApp (mkCId "defTermToTerm") [gf x1]
  gf (GqNotionToTerm x1) = mkApp (mkCId "qNotionToTerm") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "defTermToTerm" -> GdefTermToTerm (fg x1)
      Just (i,[x1]) | i == mkCId "qNotionToTerm" -> GqNotionToTerm (fg x1)


      _ -> error ("no term " ++ show t)

instance Gf Gtext where
  gf (GthmToText x1) = mkApp (mkCId "thmToText") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "thmToText" -> GthmToText (fg x1)


      _ -> error ("no text " ++ show t)

instance Gf Gvariable where
  gf GA_VAR = mkApp (mkCId "A_VAR") []
  gf GB_VAR = mkApp (mkCId "B_VAR") []
  gf GC_VAR = mkApp (mkCId "C_VAR") []
  gf GK_VAR = mkApp (mkCId "K_VAR") []
  gf GM_VAR = mkApp (mkCId "M_VAR") []
  gf GN_VAR = mkApp (mkCId "N_VAR") []
  gf GR_VAR = mkApp (mkCId "R_VAR") []
  gf GX_VAR = mkApp (mkCId "X_VAR") []
  gf GY_VAR = mkApp (mkCId "Y_VAR") []
  gf GZ_VAR = mkApp (mkCId "Z_VAR") []
  gf (GknownName x1) = mkApp (mkCId "knownName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "A_VAR" -> GA_VAR 
      Just (i,[]) | i == mkCId "B_VAR" -> GB_VAR 
      Just (i,[]) | i == mkCId "C_VAR" -> GC_VAR 
      Just (i,[]) | i == mkCId "K_VAR" -> GK_VAR 
      Just (i,[]) | i == mkCId "M_VAR" -> GM_VAR 
      Just (i,[]) | i == mkCId "N_VAR" -> GN_VAR 
      Just (i,[]) | i == mkCId "R_VAR" -> GR_VAR 
      Just (i,[]) | i == mkCId "X_VAR" -> GX_VAR 
      Just (i,[]) | i == mkCId "Y_VAR" -> GY_VAR 
      Just (i,[]) | i == mkCId "Z_VAR" -> GZ_VAR 
      Just (i,[x1]) | i == mkCId "knownName" -> GknownName (fg x1)


      _ -> error ("no variable " ++ show t)


