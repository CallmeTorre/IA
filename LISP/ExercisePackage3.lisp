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

;20)
(defun Aplana(lista)
     (cond ((null lista) NIL)
     	     ((listp (first lista)) (append (Aplana (first lista)) (Aplana (rest lista))))
     	     (T (cons (first lista) (Aplana (rest lista))))))

(Aplana '(((b) a) a (c))) ; (B A A C)

;21)
(defun Elimina(lista n)
     (cond ((null lista) NIL)
     	     ((not (numberp (first lista))) (Elimina (rest lista) n))
	         ((<=(first lista) n)(Elimina (rest lista) n))
	         (T (cons (first lista) (Elimina (rest lista) n)))))

(elimina '(1 a 2 b 3) 1) ; (2 3)

;22)
(defun PegayCambia (lista1 lista2 elem1 elem2)
       (Cambia(Pega lista1 lista2) elem1 elem2))

(defun Cambia(lista elem1 elem2)
      (cond ((null lista) NIL)
      	    ((equal (first lista) elem1) (cons elem2 (Cambia(rest lista) elem1 elem2)))
	          (T (cons (first lista) (Cambia (rest lista) elem1 elem2)))))

(defun Pega (lista1 lista2)
       (cond ((null lista1) lista2)
       	     (T (cons (first lista1) (Pega (rest lista1) lista2)))))

(PegaYCambia '(d b c d) '(d b d b) 'b 'w) ; (D W C D D W D W)
