module Blocks where

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

data Gassumption =
   GpropToAssump GhypVar Gproposition 
 | GtypeDefToAssump Gvariable Gtype 
  deriving Show

data Gblock =
   GdefBlToBl GdefBl 
 | GexmBltoBl GexampleBl 
  deriving Show

data GdefBl = GassToDefBl GString GLassumption Gproposition 
  deriving Show

data Gentity =
   Gfun1ToEntity Gfun1 Gentity 
 | Gfun2ToEntity Gfun2 Gentity Gentity 
 | Gfun3ToEnitity Gfun3 Gentity Gentity Gentity 
 | GintToEntity GInt 
 | GvarToEntity Gvariable 
  deriving Show

data GexampleBl = GassToExmBl GLassumption Gproposition 
  deriving Show

data Gfun1 = GrN1ToFun1 GrawNoun1 
  deriving Show

data Gfun2 = GrN2ToFun2 GrawNoun2 
  deriving Show

data GhypVar = GH 
  deriving Show

data Gproposition =
   Gand Gproposition Gproposition 
 | Gexists Gvariable Gtype Gproposition 
 | GforAll Gvariable Gtype Gproposition 
 | Giff Gproposition Gproposition 
 | Gimplies Gproposition Gproposition 
 | Gnot Gproposition 
 | Gor Gproposition Gproposition 
 | Grel1ToProp Grel1 Gentity 
 | Grel2ToProp Grel2 Gentity Gentity 
 | Grel3ToProp Grel3 Gentity Gentity Gentity 
  deriving Show

data GrawAdjective0 =
   GEVEN 
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

data Grel1 = GrA0ToRel1 GrawAdjective0 
  deriving Show

data Grel2 =
   GrA1ToRel2 GrawAdjective1 
 | GrAM0ToRel2 GrawAdjectiveM0 
  deriving Show

data Grel3 = GrA2ToRel3 GrawAdjective2 
  deriving Show

data Gtype = GrN0ToType GrawNoun0 
  deriving Show

data Gvariable =
   GA_VAR 
 | GB_VAR 
 | GC_VAR 
 | GK_VAR 
 | GN_VAR 
 | GR_VAR 
 | GX_VAR 
 | GY_VAR 
 | GZ_VAR 
 | GintToVar GInt 
  deriving Show

data Gfun3

data GrawAdjective2

data GrawAdjectiveM0

data GrawNoun1


instance Gf GLassumption where
  gf GBassumption = mkApp (mkCId "Bassumption") []
  gf (GCassumption x1 x2) = mkApp (mkCId "Cassumption") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Bassumption" -> GBassumption 
      Just (i,[x1,x2]) | i == mkCId "Cassumption" -> GCassumption (fg x1) (fg x2)


      _ -> error ("no Lassumption " ++ show t)

instance Gf Gassumption where
  gf (GpropToAssump x1 x2) = mkApp (mkCId "propToAssump") [gf x1, gf x2]
  gf (GtypeDefToAssump x1 x2) = mkApp (mkCId "typeDefToAssump") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "propToAssump" -> GpropToAssump (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "typeDefToAssump" -> GtypeDefToAssump (fg x1) (fg x2)


      _ -> error ("no assumption " ++ show t)

instance Gf Gblock where
  gf (GdefBlToBl x1) = mkApp (mkCId "defBlToBl") [gf x1]
  gf (GexmBltoBl x1) = mkApp (mkCId "exmBltoBl") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "defBlToBl" -> GdefBlToBl (fg x1)
      Just (i,[x1]) | i == mkCId "exmBltoBl" -> GexmBltoBl (fg x1)


      _ -> error ("no block " ++ show t)

instance Gf GdefBl where
  gf (GassToDefBl x1 x2 x3) = mkApp (mkCId "assToDefBl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "assToDefBl" -> GassToDefBl (fg x1) (fg x2) (fg x3)


      _ -> error ("no defBl " ++ show t)

instance Gf Gentity where
  gf (Gfun1ToEntity x1 x2) = mkApp (mkCId "fun1ToEntity") [gf x1, gf x2]
  gf (Gfun2ToEntity x1 x2 x3) = mkApp (mkCId "fun2ToEntity") [gf x1, gf x2, gf x3]
  gf (Gfun3ToEnitity x1 x2 x3 x4) = mkApp (mkCId "fun3ToEnitity") [gf x1, gf x2, gf x3, gf x4]
  gf (GintToEntity x1) = mkApp (mkCId "intToEntity") [gf x1]
  gf (GvarToEntity x1) = mkApp (mkCId "varToEntity") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "fun1ToEntity" -> Gfun1ToEntity (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "fun2ToEntity" -> Gfun2ToEntity (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "fun3ToEnitity" -> Gfun3ToEnitity (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1]) | i == mkCId "intToEntity" -> GintToEntity (fg x1)
      Just (i,[x1]) | i == mkCId "varToEntity" -> GvarToEntity (fg x1)


      _ -> error ("no entity " ++ show t)

instance Gf GexampleBl where
  gf (GassToExmBl x1 x2) = mkApp (mkCId "assToExmBl") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "assToExmBl" -> GassToExmBl (fg x1) (fg x2)


      _ -> error ("no exampleBl " ++ show t)

instance Gf Gfun1 where
  gf (GrN1ToFun1 x1) = mkApp (mkCId "rN1ToFun1") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rN1ToFun1" -> GrN1ToFun1 (fg x1)


      _ -> error ("no fun1 " ++ show t)

instance Gf Gfun2 where
  gf (GrN2ToFun2 x1) = mkApp (mkCId "rN2ToFun2") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rN2ToFun2" -> GrN2ToFun2 (fg x1)


      _ -> error ("no fun2 " ++ show t)

instance Gf GhypVar where
  gf GH = mkApp (mkCId "H") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "H" -> GH 


      _ -> error ("no hypVar " ++ show t)

instance Gf Gproposition where
  gf (Gand x1 x2) = mkApp (mkCId "and") [gf x1, gf x2]
  gf (Gexists x1 x2 x3) = mkApp (mkCId "exists") [gf x1, gf x2, gf x3]
  gf (GforAll x1 x2 x3) = mkApp (mkCId "forAll") [gf x1, gf x2, gf x3]
  gf (Giff x1 x2) = mkApp (mkCId "iff") [gf x1, gf x2]
  gf (Gimplies x1 x2) = mkApp (mkCId "implies") [gf x1, gf x2]
  gf (Gnot x1) = mkApp (mkCId "not") [gf x1]
  gf (Gor x1 x2) = mkApp (mkCId "or") [gf x1, gf x2]
  gf (Grel1ToProp x1 x2) = mkApp (mkCId "rel1ToProp") [gf x1, gf x2]
  gf (Grel2ToProp x1 x2 x3) = mkApp (mkCId "rel2ToProp") [gf x1, gf x2, gf x3]
  gf (Grel3ToProp x1 x2 x3 x4) = mkApp (mkCId "rel3ToProp") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "and" -> Gand (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "exists" -> Gexists (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "forAll" -> GforAll (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "iff" -> Giff (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "implies" -> Gimplies (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "not" -> Gnot (fg x1)
      Just (i,[x1,x2]) | i == mkCId "or" -> Gor (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "rel1ToProp" -> Grel1ToProp (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "rel2ToProp" -> Grel2ToProp (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3,x4]) | i == mkCId "rel3ToProp" -> Grel3ToProp (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no proposition " ++ show t)

instance Gf GrawAdjective0 where
  gf GEVEN = mkApp (mkCId "EVEN") []
  gf GNONNEGATIVE = mkApp (mkCId "NONNEGATIVE") []
  gf GODD = mkApp (mkCId "ODD") []
  gf GPOSITIVE = mkApp (mkCId "POSITIVE") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "EVEN" -> GEVEN 
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

instance Gf Grel1 where
  gf (GrA0ToRel1 x1) = mkApp (mkCId "rA0ToRel1") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rA0ToRel1" -> GrA0ToRel1 (fg x1)


      _ -> error ("no rel1 " ++ show t)

instance Gf Grel2 where
  gf (GrA1ToRel2 x1) = mkApp (mkCId "rA1ToRel2") [gf x1]
  gf (GrAM0ToRel2 x1) = mkApp (mkCId "rAM0ToRel2") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rA1ToRel2" -> GrA1ToRel2 (fg x1)
      Just (i,[x1]) | i == mkCId "rAM0ToRel2" -> GrAM0ToRel2 (fg x1)


      _ -> error ("no rel2 " ++ show t)

instance Gf Grel3 where
  gf (GrA2ToRel3 x1) = mkApp (mkCId "rA2ToRel3") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rA2ToRel3" -> GrA2ToRel3 (fg x1)


      _ -> error ("no rel3 " ++ show t)

instance Gf Gtype where
  gf (GrN0ToType x1) = mkApp (mkCId "rN0ToType") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "rN0ToType" -> GrN0ToType (fg x1)


      _ -> error ("no type " ++ show t)

instance Gf Gvariable where
  gf GA_VAR = mkApp (mkCId "A_VAR") []
  gf GB_VAR = mkApp (mkCId "B_VAR") []
  gf GC_VAR = mkApp (mkCId "C_VAR") []
  gf GK_VAR = mkApp (mkCId "K_VAR") []
  gf GN_VAR = mkApp (mkCId "N_VAR") []
  gf GR_VAR = mkApp (mkCId "R_VAR") []
  gf GX_VAR = mkApp (mkCId "X_VAR") []
  gf GY_VAR = mkApp (mkCId "Y_VAR") []
  gf GZ_VAR = mkApp (mkCId "Z_VAR") []
  gf (GintToVar x1) = mkApp (mkCId "intToVar") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "A_VAR" -> GA_VAR 
      Just (i,[]) | i == mkCId "B_VAR" -> GB_VAR 
      Just (i,[]) | i == mkCId "C_VAR" -> GC_VAR 
      Just (i,[]) | i == mkCId "K_VAR" -> GK_VAR 
      Just (i,[]) | i == mkCId "N_VAR" -> GN_VAR 
      Just (i,[]) | i == mkCId "R_VAR" -> GR_VAR 
      Just (i,[]) | i == mkCId "X_VAR" -> GX_VAR 
      Just (i,[]) | i == mkCId "Y_VAR" -> GY_VAR 
      Just (i,[]) | i == mkCId "Z_VAR" -> GZ_VAR 
      Just (i,[x1]) | i == mkCId "intToVar" -> GintToVar (fg x1)


      _ -> error ("no variable " ++ show t)

instance Show Gfun3

instance Gf Gfun3 where
  gf _ = undefined
  fg _ = undefined



instance Show GrawAdjective2

instance Gf GrawAdjective2 where
  gf _ = undefined
  fg _ = undefined



instance Show GrawAdjectiveM0

instance Gf GrawAdjectiveM0 where
  gf _ = undefined
  fg _ = undefined



instance Show GrawNoun1

instance Gf GrawNoun1 where
  gf _ = undefined
  fg _ = undefined




