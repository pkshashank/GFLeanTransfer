concrete BlocksLean of Blocks = LogicPropLean ** open Prelude, Utils in {
    lincat
        block = SS ;
        defBl = SS ;
        exampleBl = SS ;
        assumption = SS ;
        Lassumption = SS ;
    lin
        Bassumption = ss "" ;
        Cassumption = cc2 ;
    lin 
        propToAssump = isType ;
        typeDefToAssump = isType ;

        assToExmBl lass prop = {s = "example" ++ lass.s ++ ":" ++ prop.s ++ ":=" ++ "sorry"} ;

        assToDefBl tag lass prop = {s = "def" ++ tag.s ++ lass.s ++ ": Prop" ++ ":=" ++ prop.s} ;

        defBlToBl = id SS ;
        exmBltoBl = id SS ;

}