abstract LogicProp = Lexicon ** {
    flags
        startcat = proposition ;
    cat
        type ;
        entity ;
        hypVar ;
        rel1 ;
        rel2 ;
        rel3 ;
        fun1 ;
        fun2 ;
        fun3 ;
        proposition ;

    fun
        rN0ToType : rawNoun0 -> type ;

        --rN1ToFun1 : rawNoun1 -> fun1 ;
        
        rN2ToFun2 : rawNoun2 -> fun2 ;

        rA0ToRel1 : rawAdjective0 -> rel1 ;

        rA1ToRel2 : rawAdjective1 -> rel2 ;
        --rAM0ToRel2 : rawAdjectiveM0 -> rel2 ;

        --rA2ToRel3 : rawAdjective2 -> rel3 ;

        varToEntity : variable -> entity ;
        intToEntity : Int -> entity ;
        intToVar : Int -> variable ;
    
    fun
        rel1ToProp : rel1 -> entity -> proposition ;
        rel2ToProp : rel2 -> entity -> entity -> proposition ;
        rel3ToProp : rel3 -> entity -> entity -> entity -> proposition ;


        fun1ToEntity : fun1 -> entity -> entity ;
        fun2ToEntity : fun2 -> entity -> entity -> entity ;
        fun3ToEnitity : fun3 -> entity -> entity -> entity -> entity ;

        and, or, implies, iff : proposition -> proposition -> proposition ;
        not : proposition -> proposition ;
        forAll : variable -> type -> proposition -> proposition ;
        exists : variable -> type -> proposition -> proposition ;

        -- Lexicon for hypothesis variable
        H : hypVar ;

        
}