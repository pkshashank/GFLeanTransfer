resource Utils = open Prelude in {
    param
        Lang = E | L ; -- For English and Lean linearizations
    oper
        SS2 : Type = {s1, s2 : Str};
        SS3 : Type = {s1, s2, s3 : Str} ;
        SS4 : Type = {s1, s2, s3, s4 : Str} ;

        SSL : Type = Lang => SS ;

        SS2L : Type = Lang => SS2 ;
        SS3L : Type = Lang => SS3 ;
        SS4L : Type = Lang => SS4 ;

-- Usual operations for records
        -- mkSS1 is just ss
        mkSS2 : Str -> Str -> SS2 = sd2 ;

        mkSS3 : Str -> Str -> Str -> SS3 = \x,y,z -> {s1 = x ; s2 = y ; s3 = z} ;

        mkSS4 : Str -> Str -> Str -> Str -> SS4 = \a,b,c, d -> {s1 = a ; s2 = b ; s3 = c ; s4 = d} ;

-- Lin types for tables made up of records
        mkSSL = overload {
            mkSSL : Str -> Str -> SSL = \x,y -> table {E => ss x ; L => ss y} ;

            mkSSL : Str -> SSL = \x -> table {E => ss x ; L => ss x} ;
            } ;

        mkSS2L = overload {
            mkSS2L : Str -> Str -> Str -> Str -> SS2L = \e1,e2,l1,l2 -> table {E => mkSS2 e1 e2 ; L => mkSS2 l1 l2} ;

            mkSS2L : Str -> Str -> SS2L = \e1,e2 -> table {E => mkSS2 e1 e1 ; L => mkSS2 e1 e2} ;
            } ;

        mkSS3L = overload {
            mkSS3L : Str -> Str -> Str -> Str -> Str -> Str -> SS3L = \e1,e2,e3,l1,l2,l3 -> table {E => mkSS3 e1 e2 e3 ; L => mkSS3 l1 l2 l3} ;

            mkSS3L :  Str -> Str -> Str -> SS3L = \e1,e2,e3 -> table {E => mkSS3 e1 e2 e3 ; L => mkSS3 e1 e2 e3} ;
            };

        mkSS4L = overload {
            mkSS4L : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> SS4L = \e1,e2,e3,e4,l1,l2,l3,l4 -> table {E => mkSS4 e1 e2 e3 e4 ; L => mkSS4 l1 l2 l3 l4} ;

            mkSS4L : Str -> Str -> Str -> Str -> SS4L = \e1,e2,e3,e4 -> table {E => mkSS4 e1 e2 e3 e4 ; L => mkSS4 e1 e2 e3 e4} ;
            } ;

        isType : SS -> SS -> SS = \s1,s2 -> {s = "(" ++ s1.s ++ ":" ++ s2.s ++ ")"} ;
}