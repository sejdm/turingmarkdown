# Checking for equality

This state transition table checks if the string ...=... is correct. It doees so by repeatedly marking a character that it will search for, then hunts for = symbol and then begins searching for the first non marked character and checks for equality.

label: equal
| equal | 0      | 1       | X       | =       |         |
|-------|--------|---------|---------|---------|---------|
| 0     | f,X,R  | ff,X,R  | 0,X,R   | acc,=,R | re,=,L  |
| f     | f,0,R  | f,1,R   | f,1,R   | o,=,R   | re,=,L  |
| o     | be,X,R | re,o,R  | o,X,R   | re,=,R  | re,=,L  |
| ff    | ff,0,R | ff,1 ,R | ff ,X,R | oo,=,R  | re,=,L  |
| oo    | re,0,R | be,X,R  | oo,X,R  | re,=,R  | re,=,L  |
| be    | be,0,L | be,1,L  | be,X,L  | bf,=,L  | acc,E,R |
| bf    | bf,0,L | bf,1,L  | 0,X,R   | re,=,R  | acc,=,R |



label: carry
| carry   |            | 1           | 0           |
| initial | carry, ,L  | initial,1,R | initial,0,R |
| carry   | accept,1,L | carry,0,L   | accept,1,L  |


<<equal>>: 1011=1011

That is all

<<carry>>: 101111

DONE
