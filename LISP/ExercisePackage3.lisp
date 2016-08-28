;2)
(defun Inicio-en(lista elem)
      (cond ((null lista) NIL)
      	       ((equal (first lista) elem) lista)
	       (T (Inicio-en (rest lista) elem))))

(Inicio-en '(1 2 3) '2) ; (2 3)

;3)
(defun Termina-en(lista elem)
      (cond ((null lista) NIL)
      	    ((equal (first lista) elem) (cons elem nil))
	          (T (cons (first lista) (Termina-en (rest lista) elem)))))

(TERMINA-EN '(1 2 3) '2) ; (1 2)

;4)
(defun Primer-impar(lista cont)
      (cond ((null lista) nil)
      	       ((oddp (first lista)) (cons (first lista) cont))
	       (T (Primer-impar (rest lista) (+ cont 1)))))

(Primer-impar '(2 3 4 5) 0) ; (3 . 1)

;5)
(defun ImparAux (lista)
      (cond ((null lista) nil)
      	       ((>= (first lista) 0) (first lista))
	       (T (ImparAux (rest lista)))))

(defun Contar (elem lista &optional cont)
       (cond ((null lista) (list elem cont))
       	     	((equal elem (first lista)) (Contar elem (rest lista) (+ cont 1)))
		(T (Contar elem (rest lista) cont))))

(defun Ultimo-impar (lista)
       (Contar (ImparAux (reverse lista)) lista 0))

(ultimo-impar '(1 2 1)) ; (1 2)

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

;19)
(defun Mapea(funcion lista1 lista2)
      (if lista1 (cons (funcall funcion (first lista1) (first lista2))
      	  	       	   (Mapea funcion (rest lista1) (rest lista2)))
		   NIL))

(mapea #'- '(1 2 3 4) '(1 1 1 1)) ; (0 1 2 3)

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
