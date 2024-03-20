abstract Texts = Statements ** {
    flags
        startcat = text ;
    cat
        example ;
        assumption ;
        text ;

        Lassumption ; -- {0}
    fun
        Bassumption : Lassumption ;
        Cassumption : assumption -> Lassumption -> Lassumption ; 

    fun 
        assToExm : Lassumption -> statement -> example ;

        stmToAssumption : statement -> assumption ;
        thmToText : example -> text ;
}