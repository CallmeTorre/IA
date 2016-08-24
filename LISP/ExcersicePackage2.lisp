;1)
(defun ElemInPos (elem lista pos)
    (if (equal elem (nth pos lista)) T NIL))

(ELEMINPOS 'a '(a) 0) ; T
(ELEMINPOS 'b '(a j c) 4) ; NIL

;2)
(defun Inicio-en (lista elem)
    (cons (position elem lista) lista))

(inicio-en '(a b c) 'a) ; (0 A B C)

;3)
(defun Termina-en (lista elem)
       (let* ((lista_invertida (reverse lista)) ;Invertimos la Lista
       (contador (position elem lista_invertida)) ;Obtenemos la ultima
       incidencia de elem en la lista
       (tamaño (- (length lista)1)) ;Obtenemos el tamaño -1 de la lista
       (final (- tamaño contador)) ;Obtenemos hasta donde tenemos que
       ciclar
       (aux 0))
       (loop for i in lista
       	    do(setq aux(+ aux 1))
	    collect(list i)
	    until(equal aux final))))

(TERMINA-EN '(a b c a c a) 'a) ;((A) (B) (C) (A) (C))

;4)
(defun Primer-impar (lista)
       (let ((contador -1))
       (loop for i in lista
       	     do(setq contador(+ contador 1))
       	     if (oddp i)
       	     	return (list i contador))))

(Primer-impar '(2 3 4 5)) ; (3 1)

;5)
(defun Ultimo-impar (lista)
       (let* ((incidencias 0)
       	        (ultimo 0)
		(contador 0))
       (dolist (i (reverse lista))
       	       (if (>= i 0) (setq contador(+ contador 1)))
	       (if (and (>= i 0) (equal contador 1)) (setq ultimo i)))
       (dolist (x lista)
       	       (if (equal x ultimo) (setq incidencias(+ incidencias 1))))
       (list ultimo incidencias)))

(ultimo-impar '(1 2 3 8 8 8 -3)) ; (8 3)

;6)
(defun Conteo (lista)
       (let* ((elem_num 0)
       	       (sublistas 0))
       (dolist (i lista)
       	       (if (numberp i) (setq elem_num(+ elem_num 1)))
	       (if (listp i) (setq sublistas(+ sublistas 1))))
	       (list elem_num sublistas)))

(Conteo '(2 a b c (a d) 1 (a b c 2) 3)) ; (3 2)

;7)
(defun Aplana (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (Aplana (cdr l))))
        (t (append (Aplana (car l)) (Aplana (cdr l))))))

(Aplana '((a b) c d ((e f g)) h)) ; (A B C D E F G H)
;8)
(defun Diagonal (lista)
    (loop for i from 0
    	    for elem in lista
	    collect(nth i elem)))

(diagonal '((a b c) (a b c) (a b c))) ;(A B C)

;9)
(defun Analiza (lista)
       (let ((lista_nueva '()))
       	(dolist (i lista (reverse lista_nueva))
		(cond ((null i) (setq lista_nueva(cons 'N lista_nueva)))
		      ((listp i) (setq lista_nueva(cons 'L lista_nueva)))
			    (T (setq lista_nueva(cons 'A lista_nueva)))))))

(Analiza '(a () 2 3 (a b c))) ; (A N A A L)

;10)
(defun Suma-numerica (lista)
       (loop for i in lista
       	     if(numberp i)
	            sum i))

(SUMA-numerica '(1 2 3 a b 2 c 3)) ; 11

;11)
(defun Filtra-múltiplos (lista elem)
      (loop for i in lista
      	    if(not(equal 0 (mod i elem)))
	           collect i))

(filtra-múltiplos '(1 2 3 4 5 6) 2) ; (1 3 5)

;14)
(defun Implica (&rest argumentos)
             (every #'identity argumentos))

(Implica T T) ;=> T
