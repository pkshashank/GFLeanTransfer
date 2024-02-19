abstract Lexicon = {
    cat
        variable ;
        rawNoun0 ;
        rawNoun1 ;
        rawNoun2 ;

        rawAdjective0 ;
        rawAdjective1 ;
        rawAdjective2 ;
        rawAdjectiveM0 ;


    fun -- Real Lexicon starts here 
        A_VAR : variable ;
        B_VAR : variable ;
        C_VAR : variable ;
        K_VAR : variable ;
        M_VAR : variable ;
        N_VAR : variable ;
        R_VAR : variable ;
        X_VAR : variable ;
        Y_VAR : variable ;
        Z_VAR : variable ;

        REAL_NUMBER : rawNoun0 ;
        INTEGER : rawNoun0 ;
        RATIONAL : rawNoun0 ;

        LESS_THAN : rawAdjective1 ;
        LESS_TE : rawAdjective1 ;
        GREATER_THAN : rawAdjective1 ;
        GREATER_TE : rawAdjective1 ;
        NOT_EQUAL : rawAdjective1 ;
        EQUAL : rawAdjective1 ;
        
        --BETWEEN : rawAdjective2 ;

        POSITIVE : rawAdjective0 ;
        ODD : rawAdjective0 ;
        EVEN : rawAdjective0 ;
        NONNEGATIVE : rawAdjective0 ;
        NEGATIVE: rawAdjective0 ;

        --SAMEPARITY : rawAdjectiveM0 ;
        --OPPPARITY : rawAdjectiveM0 ; 

        EXP : rawNoun2 ;
        SUM : rawNoun2 ;
        MINUS : rawNoun2 ;
        PRODUCT : rawNoun2 ;
        FRAC : rawNoun2 ;

}
