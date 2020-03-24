# Checking for equality

This state transition table checks if the string ...=... is correct. 1t doees so by repeatedly marking a character that it will search for, then hunts for = symbol and then begins searching for the first non marked character and checks for equality.

|  |0|1|X|=|_ |
|--|-|-|-|-|-|
| 0|(f, X, R)|(ff, X, R)|(0, X, R)|(Accept, =, R)|(Reject, =, L) |
| f|(f, 0, R)|(f, 1, R)|(f, 1, R)|(o, =, R)|(Reject, =, L) |
| o|(be, X, R)|(Reject, o, R)|(o, X, R)|(Reject, =, R)|(Reject, =, L) |
| ff|(ff, 0, R)|(ff, 1, R)|(ff, X, R)|(oo, =, R)|(Reject, =, L) |
| oo|(Reject, 0, R)|(be, X, R)|(oo, X, R)|(Reject, =, R)|(Reject, =, L) |
| be|(be, 0, L)|(be, 1, L)|(be, X, L)|(bf, =, L)|(Accept, E, R) |
| bf|(bf, 0, L)|(bf, 1, L)|(0, X, R)|(Reject, =, R)|(Accept, =, R) |


|  |_|1|0 |
|--|-|-|-|
| initial|(carry, _, L)|(initial, 1, R)|(initial, 0, R) |
| carry|(Accept, 1, L)|(carry, 0, L)|(Accept, 1, L) |

input: 1011=1011
```
|1| 0  1  1  =  1  0  1  1             0:  (0, 1) -> (ff, X, R)
 X |0| 1  1  =  1  0  1  1             ff:  (ff, 0) -> (ff, 0, R)
 X  0 |1| 1  =  1  0  1  1             ff:  (ff, 1) -> (ff, 1, R)
 X  0  1 |1| =  1  0  1  1             ff:  (ff, 1) -> (ff, 1, R)
 X  0  1  1 |=| 1  0  1  1             ff:  (ff, =) -> (oo, =, R)
 X  0  1  1  = |1| 0  1  1             oo:  (oo, 1) -> (be, X, R)
 X  0  1  1  =  X |0| 1  1             be:  (be, 0) -> (be, 0, L)
 X  0  1  1  = |X| 0  1  1             be:  (be, X) -> (be, X, L)
 X  0  1  1 |=| X  0  1  1             be:  (be, =) -> (bf, =, L)
 X  0  1 |1| =  X  0  1  1             bf:  (bf, 1) -> (bf, 1, L)
 X  0 |1| 1  =  X  0  1  1             bf:  (bf, 1) -> (bf, 1, L)
 X |0| 1  1  =  X  0  1  1             bf:  (bf, 0) -> (bf, 0, L)
|X| 0  1  1  =  X  0  1  1             bf:  (bf, X) -> (0, X, R)
 X |0| 1  1  =  X  0  1  1             0:  (0, 0) -> (f, X, R)
 X  X |1| 1  =  X  0  1  1             f:  (f, 1) -> (f, 1, R)
 X  X  1 |1| =  X  0  1  1             f:  (f, 1) -> (f, 1, R)
 X  X  1  1 |=| X  0  1  1             f:  (f, =) -> (o, =, R)
 X  X  1  1  = |X| 0  1  1             o:  (o, X) -> (o, X, R)
 X  X  1  1  =  X |0| 1  1             o:  (o, 0) -> (be, X, R)
 X  X  1  1  =  X  X |1| 1             be:  (be, 1) -> (be, 1, L)
 X  X  1  1  =  X |X| 1  1             be:  (be, X) -> (be, X, L)
 X  X  1  1  = |X| X  1  1             be:  (be, X) -> (be, X, L)
 X  X  1  1 |=| X  X  1  1             be:  (be, =) -> (bf, =, L)
 X  X  1 |1| =  X  X  1  1             bf:  (bf, 1) -> (bf, 1, L)
 X  X |1| 1  =  X  X  1  1             bf:  (bf, 1) -> (bf, 1, L)
 X |X| 1  1  =  X  X  1  1             bf:  (bf, X) -> (0, X, R)
 X  X |1| 1  =  X  X  1  1             0:  (0, 1) -> (ff, X, R)
 X  X  X |1| =  X  X  1  1             ff:  (ff, 1) -> (ff, 1, R)
 X  X  X  1 |=| X  X  1  1             ff:  (ff, =) -> (oo, =, R)
 X  X  X  1  = |X| X  1  1             oo:  (oo, X) -> (oo, X, R)
 X  X  X  1  =  X |X| 1  1             oo:  (oo, X) -> (oo, X, R)
 X  X  X  1  =  X  X |1| 1             oo:  (oo, 1) -> (be, X, R)
 X  X  X  1  =  X  X  X |1|            be:  (be, 1) -> (be, 1, L)
 X  X  X  1  =  X  X |X| 1             be:  (be, X) -> (be, X, L)
 X  X  X  1  =  X |X| X  1             be:  (be, X) -> (be, X, L)
 X  X  X  1  = |X| X  X  1             be:  (be, X) -> (be, X, L)
 X  X  X  1 |=| X  X  X  1             be:  (be, =) -> (bf, =, L)
 X  X  X |1| =  X  X  X  1             bf:  (bf, 1) -> (bf, 1, L)
 X  X |X| 1  =  X  X  X  1             bf:  (bf, X) -> (0, X, R)
 X  X  X |1| =  X  X  X  1             0:  (0, 1) -> (ff, X, R)
 X  X  X  X |=| X  X  X  1             ff:  (ff, =) -> (oo, =, R)
 X  X  X  X  = |X| X  X  1             oo:  (oo, X) -> (oo, X, R)
 X  X  X  X  =  X |X| X  1             oo:  (oo, X) -> (oo, X, R)
 X  X  X  X  =  X  X |X| 1             oo:  (oo, X) -> (oo, X, R)
 X  X  X  X  =  X  X  X |1|            oo:  (oo, 1) -> (be, X, R)
 X  X  X  X  =  X  X  X  X | |            be:  (be, _) -> (Accept, E, R)
 X  X  X  X  =  X  X  X  X  E | |            Accept
```


That is all

input: 101111
```
|1| 0  1  1  1  1             initial:  (initial, 1) -> (initial, 1, R)
 1 |0| 1  1  1  1             initial:  (initial, 0) -> (initial, 0, R)
 1  0 |1| 1  1  1             initial:  (initial, 1) -> (initial, 1, R)
 1  0  1 |1| 1  1             initial:  (initial, 1) -> (initial, 1, R)
 1  0  1  1 |1| 1             initial:  (initial, 1) -> (initial, 1, R)
 1  0  1  1  1 |1|            initial:  (initial, 1) -> (initial, 1, R)
 1  0  1  1  1  1 | |            initial:  (initial, _) -> (carry, _, L)
 1  0  1  1  1 |1| _             carry:  (carry, 1) -> (carry, 0, L)
 1  0  1  1 |1| 0  _             carry:  (carry, 1) -> (carry, 0, L)
 1  0  1 |1| 0  0  _             carry:  (carry, 1) -> (carry, 0, L)
 1  0 |1| 0  0  0  _             carry:  (carry, 1) -> (carry, 0, L)
 1 |0| 0  0  0  0  _             carry:  (carry, 0) -> (Accept, 1, L)
|1| 1  0  0  0  0  _             Accept
```


DONE
