concrete LexiconEng of Lexicon = Utils ** open Prelude, Formal in {
    lincat

        variable = SS ;
        rawNoun0 = SSL ;
        rawNoun1 = SS2L ;
        rawNoun2 = TermPrec ;

        rawAdjective0 = SSL ;
        rawAdjective1 = SS2L ;
        rawAdjective2 = SS3L ;
        rawAdjectiveM0 = SSL ;


    lin
        A_VAR = ss "a" ;
        B_VAR = ss "b" ;
        C_VAR = ss "c" ;
        K_VAR = ss "k" ;
        M_VAR = ss "m" ;
        N_VAR = ss "n" ;
        R_VAR = ss "r" ;
        X_VAR = ss "x" ;
        Y_VAR = ss "y" ;
        Z_VAR = ss "z" ;

        REAL_NUMBER = mkSSL ("real" ++ ("number" | "numbers")) "ℝ" ;
        INTEGER = mkSSL ("integer" | "integers") "ℤ" ;
        RATIONAL = mkSSL ("rational" ++ ("number"| "numbers"))  "ℚ" ;
 
        LESS_THAN = mkSS2L "less than" "" "<" "" ;
        LESS_TE = mkSS2L "less than or equal to" "" "≤" "" ;
        GREATER_THAN = mkSS2L "greater than" "" ">" "" ;
        GREATER_TE = mkSS2L "greater than or equal to" "" "≥" "" ;
        NOT_EQUAL = mkSS2L "not equal to" "" "≠" "" ;
        EQUAL = mkSS2L "equal to" "" "=" "" ;
        --BETWEEN  = mkSS3L "between" "and" "" ;

        POSITIVE = mkSSL "positive" "pos" ;
        ODD = mkSSL "odd" "odd" ;
        EVEN = mkSSL "even" "even" ;
        NONNEGATIVE = mkSSL "nonnegative" "nneg" ;
        NEGATIVE = mkSSL "negative" "neg" ;

        --SAMEPARITY = mkSSL "have same parity" "samePar" ;
        --OPPPARITY = mkSSL "have opposite parity" "oppPar" ;

        EXP = mkPrec 4 "^";
        SUM = mkPrec 0 "+";
        MINUS = mkPrec 1 "-";
        PRODUCT = mkPrec 2 "*";
        FRAC = mkPrec 3 "/";
}
