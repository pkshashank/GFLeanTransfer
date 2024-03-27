concrete LogicPropLean of LogicProp = LexiconEng ** open Prelude in {
    lincat
        type = SS ;
        entity = SS ;
        hypVar = SS ;

        rel1 = {s1, s2 : Str} ;
        rel2 = {s1, s2, s3 : Str} ;
        rel3 = {s1, s2, s3, s4 : Str} ;
        fun1 = {s1, s2 : Str} ;
        fun2 = SS ;
        fun3 = {s1, s2, s3, s4 : Str} ;

        proposition = SS ;
    lin
        rN0ToType rN0 = rN0!L ;

        --rN1ToFun1 rN1 = rN1!L ;

        rN2ToFun2 rn2 = ss rn2.s  ;

        rA0ToRel1 rAdj = {s1 = (rAdj!L).s ; s2 = "" } ;

        rA1ToRel2 rAdj = {s1 = "" ; s2 = (rAdj!L).s1 ; s3 = (rAdj!L).s2} ;

        --rAM0ToRel2 rAdjM = {s1 = (rAdjM!L).s ; s2 = "" ; s3 = "" } ;

        --rA2ToRel3 rAdj2 = {s1 = "" ; s2 = (rAdj2!L).s1 ; s3 = (rAdj2!L).s2 ; s4 = (rAdj2!L).s3} ;

        varToEntity = id SS ;
        intToEntity = id SS ;
        intToVar x = ss ("x" ++ BIND ++ x.s) ;
     lin
        rel1ToProp r e = {s = r.s1 ++ e.s ++ r.s2} ;

        rel2ToProp r e1 e2 = {s = r.s1 ++ e1.s ++ r.s2 ++ e2.s ++ r.s3} ;

        rel3ToProp r e1 e2 e3 = {s = r.s1 ++ e1.s ++ r.s2 ++ e2.s ++ r.s3 ++ e3.s ++ r.s4} ;

        fun1ToEntity f e = {s = f.s1 ++ e.s ++ f.s2 ; p = 4} ;
        fun2ToEntity f e1 e2 = { s = paren (e1.s ++ f.s ++ e2.s) }; 
        fun3ToEnitity f e1 e2 e3 = {s = f.s1 ++ e1.s ++ f.s2 ++ e2.s ++ f.s3 ++ e3.s ++ f.s4 ; p = 4} ;

        and p1 p2 = parenss (infixSS "∧" p1 p2) ;
        or p1 p2 = parenss (infixSS "∨" p1 p2) ;
        implies p1 p2 = parenss (infixSS "→" p1 p2) ;
        iff p1 p2 = parenss (infixSS "↔" p1 p2) ;
        not p = parenss (prefixSS "¬" p) ;
        forAll v t p = {s = "∀ (" ++ v.s ++ ":" ++ t.s ++ ")," ++ p.s  } ;
        exists v t p = {s = "∃ (" ++ v.s ++ ":" ++ t.s ++ ")," ++ p.s  } ;

        -- Lexicon for hypothesis variable
        H = ss "h?" ;
}
