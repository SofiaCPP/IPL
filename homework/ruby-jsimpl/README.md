# RUBY JSIMPL

## Grammar Subset
```
PROGRAM : STATEMENT*

STATEMENT : STATEMENT if EXPRESSION
          | STATEMENT unless EXPRESSION
          | STATEMENT while EXPRESSION
          | STATEMENT until EXPRESSION
          | if EXPRESSION
              PROGRAM
            [(
            elsif EXPRESSION
              PROGRAM
            )*]
            [
            else EXPRESSION
              PROGRAM
            ]
            end
          | unless EXPRESSION
              PROGRAM
            [
            else
              PROGRAM
            ]
            end
          | case EXPRESSION
              [(when EXPRESSION : PROGRAM)*]
              [else : PROGRAM]
            end
          | while EXPRESSION
              PROGRAM
            end
          | until EXPRESSION
              PROGRAM
            end
          | for IDENTIFIER in EXPRESSION do
              PROGRAM
            end
          | class IDENTIFIER
              PROGRAM
            end
          | module IDENTIFIER
              PROGRAM
            end
          | def IDENTIFIER \( ARGUMENTS \)
              PROGRAM
            end
          | EXPRESSION

EXPRESSION  :  UNARY_EXPRESSION
            |  BINARY_EXPRESSION
            |  CALL
            |  LITERAL

UNARY_EXPRESSION  : ! EXPRESSION
                  | not EXPRESSION
                  | + EXPRESSION
                  | - EXPRESSION

BINARY_EXPRESSION : EXPRESSION or EXPRESSION
                  | EXPRESSION and EXPRESSION
                  | EXPRESSION || EXPRESSION
                  | EXPRESSION && EXPRESSION
                  | EXPRESSION + EXPRESSION
                  | EXPRESSION += EXPRESSION
                  | EXPRESSION - EXPRESSION
                  | EXPRESSION -= EXPRESSION
                  | EXPRESSION * EXPRESSION
                  | EXPRESSION *= EXPRESSION
                  | EXPRESSION ** EXPRESSION
                  | EXPRESSION **= EXPRESSION
                  | EXPRESSION / EXPRESSION
                  | EXPRESSION /= EXPRESSION
                  | EXPRESSION % EXPRESSION
                  | EXPRESSION %= EXPRESSION
                  | EXPRESSION = EXPRESSION
                  | EXPRESSION == EXPRESSION

CALL  : IDENTIFIER [\( ARGUMENTS \)] [BLOCK]

IDENTIFIER  : [a-zA-Z_][a-zA-Z0-9_]*[:: IDENTIFIER]*[. IDENTIFIER]*

LITERAL : nil
        | number
        | STRING
        | SYMBOL
        | LIST
        | HASH

BLOCK : do [| ARGUMENTS |]
          PROGRAM
        end

ARGUMENTS : IDENTIFIER [(, IDENTIFIER)*]

STRING  : 'characters*'
        | "characters*"

SYMBOL  : :IDENTIFIER
        | :STRING

LIST  : \[ [EXPRESSION, [(, EXPRESSION)*] \]

HASH  : { [IDENTIFIER => EXPRESSION, [(, IDENTIFIER => EXPRESSION)*]] }
```
