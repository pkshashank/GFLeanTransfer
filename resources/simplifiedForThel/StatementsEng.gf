    concrete StatementsEng of Statements =  PredicatesEng ** open Prelude in {
   
    lin
        andStm = infixSS ("and" | ",") ;
        orStm = infixSS "or" ;
        ifThenStm s1 s2 = cc2 (ss "if") (infixSS "then" s1 s2) ;
        iffStm = infixSS "iff" ;
        notStm = prefixSS "it's not that" ;
        qNotStmToStm qn stm = cc2 (ss "for") (infixSS "," qn stm );


        termDoesPredToStm = cc2 ;

        notionsToStm = prefixSS ("there exist" | "there exists a" | "there exists an") ;
        notionNoToStm = prefixSS ("there exists no" | "there exist no") ;
}