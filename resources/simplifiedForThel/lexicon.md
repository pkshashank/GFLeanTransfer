x : var ;
y : var ;
z : var ;
r : var ;
a : var ;
b : var ;
c : var ;
n : var ;

Real_number : primClassNoun ;
integer : primClassNoun ;

less_than : term -> primAdjective ;
less_te : term -> primAdjective ;
greater_than : term -> primAdjective ;
between : term -> term -> primAdjective ;
not_equal : term -> primAdjective ;
equal : term -> primAdjective ;

positive : primSimpleAdjective ;
odd : primSimpleAdjective ;
even : primSimpleAdjective ;
nonNegative : primSimpleAdjective ;

sameParity : primSimpleAdjectiveM ;
oppParity : primSimpleAdjectiveM ;



exp : term -> term -> primDefiniteNoun ;
sum : term -> term -> primDefiniteNoun ;
minus : term -> term -> primDefiniteNoun ;
product : term -> term -> primDefiniteNoun ;
frac : term -> term -> primDefiniteNoun ;

