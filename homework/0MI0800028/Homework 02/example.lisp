(defvar *NAME* "John")
(defvar *AGE* 23)
(defvar *ABOUT* " T NIL cond decf block")

;;; This is a stupid funciton
(defun adult? ()
  (>= *AGE* 18))

(defun will-I-graduate? ()
  NIL)

(defun distance (p1 p2)
  (sqrt (* p1 p1) (* p2 p2)))

(defun bar (a b)
  (if (and (> b a) (<= 10 b))
    (format t "Valid")
    (format t "Invalid")))

(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
