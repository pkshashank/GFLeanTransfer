abstract Predicates = Terms ** {
    cat
        polarity ;

        hasPredicate ;
        is_aPredicate ;

        possessedNoun ;

        primAdjective ; -- Adjectives with argument places, e.g. prime, dividing [term], equal to [term], etc.
        primAdjectiveM ; -- Adjectives working on plurals with argument places , e.g. equal, adjacent in [term], etc.
        primVerb ; -- converge, divides [term], belongs to [term], etc.
        primVerbM ; -- collide, commute in [term]
        primPossessedNoun ; -- nouns that occur in hasPredicate like elements [names], solutions [names] etc.

    fun
        pos : polarity ;
        neg : polarity ;

    fun
        rA0ToPAdj : rawAdjective0 -> primAdjective ;
        rA1ToPAdj : rawAdjective1 -> term -> primAdjective ;
        rA2ToPAdj : rawAdjective2 -> term -> term -> primAdjective ;

    fun
        prVerbToDPred : polarity -> primVerb -> doesPredicate ;
        prVerbMToDPred : polarity -> primVerbM -> doesPredicate ;
        hasPredToDPred : hasPredicate -> doesPredicate ;
        isPredToDPred : isPredicate -> doesPredicate ;
        isAPredToDPred : is_aPredicate -> doesPredicate ;

        primAdjToIsPred : polarity -> primAdjective -> isPredicate ;
        
        clNounToIs_aPred : polarity -> notion -> is_aPredicate ;
        deftrmToIs_aPred : polarity -> definiteTerm -> is_aPredicate ;

        possNTohasPred : possessedNoun -> hasPredicate ;
        possNnotTohasPred : possessedNoun -> hasPredicate ;

        primPosNounToPosNoun : leftAttribute -> primPossessedNoun -> possessedNoun ;
        primPosNounRAttrToPosNoun : leftAttribute -> primPossessedNoun -> rightAttribute -> possessedNoun ;


}