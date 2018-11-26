[
  A brainfuck program for doing a single digit addition.

  Ex. input: '13' -> output: '4'
      input: '99' -> output: '18'

  Author: Florian Baierl
]

initialize #0 with 48 (ASCII char for '0')
>++++ ++++
[
<++++ ++
>-
]

save input to #1 and #2
,>,

substract value from #0 from #1 and #2
<<
[
>-
>-
<<-
]

move to #1
>

substract from #1 and add to #2; now the answer is in #2
[
 ->+<
]

since we need to modify the answer afterwards write it to #3 and #6
as well
>
[>+>>>+<<<<-]


Is the answer bigger than 9?
to continue the memory tape should look like this:
0 1 0 (a) b 0
with the pointer pointing to a

<+
>>>+++++ +++++ b is 10
<              point to a

+>+<           necessary in case a and b are zero

loop to determine whether a or b reach 0 first
[->-[>]<<]

<[-
   a was bigger or equals b
   the answer is still stored in #6

   subtract 10 from #6 (the answer)
   >>>>> ----- -----

   write 48 to #4
   <++++ ++++
   [
   <++++ ++
   >-
   ]
   add 48 to #5 and #6
   <
   [->+>+<<]

   print out the results
   >+.>.

   leave loop (back to #2)
   <<<<
 ]
 <[-
   a was samller so we can simply print the answer out after adding 48 to it
   the answer is still stored in #6

   >>
   ++++ ++++
   [
   <++++ ++
   >-
   ]

   <
   [
    ->>>>+<<<<
   ]

   print #2
   >>>>.

   we want to leave the loop so go somewhere with the value '0'
   >
]
