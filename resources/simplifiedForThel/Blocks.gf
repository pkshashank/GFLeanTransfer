abstract Blocks = LogicProp ** {
    flags
        startcat = block ;
    cat
        block ;
        exampleBl ;
        assumption ;
        Lassumption ; -- {0}

    fun
        Bassumption : Lassumption ;
        Cassumption : assumption -> Lassumption -> Lassumption ;
    fun
        propToAssump : hypVar -> proposition -> assumption ; 
        typeDefToAssump : variable -> type -> assumption ;
    
        assToExmBl : Lassumption -> proposition -> exampleBl ;

        exmBltoBl : exampleBl -> block ; 

}