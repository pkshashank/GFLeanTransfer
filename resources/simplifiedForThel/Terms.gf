abstract Terms = Notions ** {
    cat
        term ;

        quantifiedNotion ;

        definiteTerm ;

        primDefiniteNoun ; -- zeroes, orders of [terms]


    fun
        rN1ToPDNoun : rawNoun1 -> term -> primDefiniteNoun ;
        rN2ToPDNoun : rawNoun2 -> term -> term -> primDefiniteNoun ;
    fun
        qNotionToTerm : quantifiedNotion -> term ;

        allNotion, someNotion, noNotion : notion -> quantifiedNotion ;

        prDefNounToDefTerm : primDefiniteNoun -> definiteTerm ;

        varToDefTerm : variable -> definiteTerm ;
        intToDefTerm : Int -> definiteTerm ;

        defTermToTerm : definiteTerm -> term ;


}