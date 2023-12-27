concrete NotionsEng of Notions = LexiconEng ** open Prelude in {
    lincat
        notion = SS ;
        names = SS ;
        
        leftAttribute = SS ;
        rightAttribute = SS ;
        isPredicate = SS ;
        doesPredicate = SS ;
        statement = SS ;
        primSimpleAdjective = SS ;
        primSimpleAdjectiveM = SS ;
        primClassNoun = SS ;

    lin

        rA0ToPSAdj rAdj = rAdj!E ;
        rN0ToPcNoun rN0 nam = cc2 (rN0!E) nam ;
        
    lin
        listVarToName = id SS ;
        knownName n = parenss (prefixSS "x" n) | ss "" ;
        

        prSimpAdjToLAttrib = id SS ;

        isPrToRAttr = id SS ;
        doesPrToRAttr = cc2 (ss "that");
        stmToRAttr = cc2 (ss "such that");


        prClNounToNotion = id SS ;
        prClNounRAttrToNotion = cc2 ;
        prLAttrClNounToNotion = cc2 ;
        prLAttrClNounRAttrToNotion = cc3 ;

}