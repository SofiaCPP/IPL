(define (example arg1 arg2)
  (if ((< arg1 arg2) and (even? arg1))
    arg1
    arg2))