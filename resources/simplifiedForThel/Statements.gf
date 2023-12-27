abstract Statements = Predicates ** {
    flags
        startcat = statement ;
    fun
        andStm, orStm, ifThenStm, iffStm : statement -> statement -> statement ;
        notStm : statement -> statement ;

        qNotStmToStm : quantifiedNotion -> statement -> statement ;
        termDoesPredToStm : term -> doesPredicate -> statement ;
        
        notionsToStm : notion -> statement ;
        notionNoToStm : notion -> statement ;

} 