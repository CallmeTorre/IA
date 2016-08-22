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
