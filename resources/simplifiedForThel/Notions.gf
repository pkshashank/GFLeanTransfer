abstract Notions = Lexicon **  {
    cat 
        notion ;

        names ;

        variable ;

        leftAttribute ;

        rightAttribute ;
        
        isPredicate ;

        doesPredicate ;

        statement ;
        
        primSimpleAdjective ; -- Adjectives with no argument places, e.g. prime, empty, etc.
        --primSimpleAdjectiveM ; -- Adjectives which are defined for plurals but take no argument, e.g. equal, parallel, disjoint etc.
        primClassNoun ; -- e.g. elements [names] of [terms]


  
    fun 
        rA0ToPSAdj : rawAdjective0 -> primSimpleAdjective ;

        rN0ToPcNoun : rawNoun0 -> names -> primClassNoun ;

        listVarToName : variable -> names ;
        knownName : Int -> variable ;

        prSimpAdjToLAttrib : primSimpleAdjective -> leftAttribute ;
        
        isPrToRAttr : isPredicate -> rightAttribute ;
        doesPrToRAttr : doesPredicate -> rightAttribute ;
        stmToRAttr : statement -> rightAttribute ;

        prClNounToNotion : primClassNoun -> notion ;
        prClNounRAttrToNotion : primClassNoun -> rightAttribute -> notion ;
        prLAttrClNounToNotion : leftAttribute -> primClassNoun -> notion ;
        prLAttrClNounRAttrToNotion : leftAttribute -> primClassNoun -> rightAttribute -> notion ;
}