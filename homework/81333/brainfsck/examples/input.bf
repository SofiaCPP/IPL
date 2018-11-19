> +
[ - >,>+<
  ----- -----    ; minus 10
  [              ; if enters means it is not a \n
    +++++ +++++  ; restore prev value
    < 
  ] >>           ; moving forward
]
                 ; numbers are 0 0 49 0 50 0 51
                 ; for input 123
<<<<[<<]         ; moving to the beginning
>>               ; reaching first char
[.>>]            ; just printing till end
