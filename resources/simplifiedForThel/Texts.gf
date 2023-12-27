abstract Texts = Statements ** {
    flags
        startcat = text ;
    cat
        definition ;
        example ;
        definiens ;
        definiendum ;
        assumption ;
        text ;

        Lassumption ; -- {0}
    fun
        Bassumption : Lassumption ;
        Cassumption : assumption -> Lassumption -> Lassumption ; 

    fun 
        assToDefn : Lassumption -> definiendum -> definiens -> definition ;

        assToExm : Lassumption -> statement -> example ;

        stmToAssumption : statement -> assumption ;

        stmToDefdum : statement -> definiendum ;
        
        stmToDefin : statement -> definiens ;

        defnToText : definition -> text ;
        thmToText : example -> text ;
}