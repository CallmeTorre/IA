;1)
(defun ElemInPos (elem lista pos)
    (if (equal elem (nth pos lista)) T NIL))

(ELEMINPOS 'a '(a) 0) ; T
(ELEMINPOS 'b '(a j c) 4) ; NIL

;2)
(defun Inicio-en (lista elem)
    (cons (position elem lista) lista))

(inicio-en '(a b c) 'a) ; (0 A B C)

;4)
(defun Primer-impar (lista)
       (let ((contador -1))
       (loop for i in lista
       	     do(setq contador(+ contador 1))
       	     if (oddp i)
       	     	return (list i contador))))

(Primer-impar '(2 3 4 5)) ; (3 1)
