concrete TextsEng of Texts = StatementsEng ** open Prelude in {
    lincat
        definition = SS ;
        example = SS ;
        definiens = SS ;
        definiendum = SS ;
        assumption = SS ;
        text = SS ;

        Lassumption = SS ;

    lin
        Bassumption = ss "";
        Cassumption as ls = prefixSS "assume" (cc2 as ls);

        assToDefn las defdum defin = {s = "defn ." ++ las.s ++ "we say" ++ defdum.s ++ "iff" ++ defin.s ++ "." } ;

        assToExm las st  = {s = "ex ." ++ las.s ++ (optStr "then") ++ st.s ++ "."} ;

        stmToAssumption = postfixSS "." ;
        
        stmToDefdum = id SS ;
        stmToDefin = id SS ;

        defnToText = id SS ;
        thmToText = id SS;


}