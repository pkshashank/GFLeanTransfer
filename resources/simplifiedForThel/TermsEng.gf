concrete TermsEng of Terms = NotionsEng ** open Prelude in {
    lincat
        term = SS ;
        quantifiedNotion = SS ;
        definiteTerm = SS ;
        constant = SS ;
        
        primDefiniteNoun = SS ;

    lin 
        rN1ToPDNoun r t = {s = (r!E).s1 ++ t.s ++ (r!E).s2} ;
        rN2ToPDNoun r t1 t2 = {s = (r!E).s1 ++ t1.s ++ (r!E).s2 ++ t2.s ++ (r!E).s3} ;

    lin
       

        qNotionToTerm = id SS ;

        allNotion = cc2 (ss "every") ;
        someNotion = cc2 (ss "some") ;
        noNotion = cc2 (ss "no") ;

        prDefNounToDefTerm = id SS ;
        
        varToDefTerm = id SS ;
        intToDefTerm = id SS ;

        defTermToTerm = id SS ;
        
}