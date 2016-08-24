;16)
(defun Encuentra(lista elem)
      (cond ((null lista) NIL)
      	    ((equal (first lista) elem) lista)
	          (T (Encuentra (rest lista) elem))))

(Encuentra  '(b c d a  c b) 'a) ; (A C B)

;17)
(defun Cambia(lista elem1 elem2)
      (cond ((null lista) NIL)
      	    ((equal (first lista) elem1) (cons elem2 (Cambia(rest lista) elem1 elem2)))
	          (T (cons (first lista) (Cambia (rest lista) elem1 elem2)))))

(Cambia '(a b c a b c) 'a '1) ; (1 B C 1 B C)
