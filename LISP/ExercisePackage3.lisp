;Jesús Alexis Torreblanca Faces

;1)
(defun ElemInPos (elem lista pos cont)
       (cond ((null lista) nil)
       	     ((and (equal pos cont)(equal (first lista) elem)) T)
		         (T (ElemInPos elem (rest lista) pos (+ cont 1)))))

(ElemInPos 'a '(a b c d) 2 0) ; NIL
(ElemInPos 'a '(a b c d) 0 0) ; T

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

;6)
(defun Conteo (lista num sublis)
       (cond ((null lista) (list num sublis))
       	     	((listp (first lista)) (Conteo (rest lista) (+ sublis 1) sublis))
		          ((numberp (first lista)) (Conteo (rest lista) num (+ num 1)))
		          (T (Conteo (rest lista) num sublis))))

 (Conteo '(1 2 3 (1)) 0 0) ; (1 3)

 ;7)
 (defun Aplana (lista)
       (cond ((null lista) nil)
       	     	((atom (first lista)) (cons (first lista) (Aplana (rest lista))))
		          (T (append (Aplana (first lista)) (Aplana (rest lista))))))

(Aplana '(a (b c) ((a)) x)) ; (A B C A X)

;8)
(defun Diagonal (lista cont)
       (cond ((null lista) nil)
       	     	(T (cons (nth cont (first lista)) (Diagonal (rest lista) (+ cont 1))))))

(diagonal '((a b c) (a b c) (a b c)) 0) ; (A B C)

;9)
(defun Analiza (lista)
       (cond ((null lista) nil)
       	     	((null (first lista)) (cons 'N (Analiza (rest lista))))
		          ((listp (first lista)) (cons 'L (Analiza (rest lista))))
		          (T (cons 'A (Analiza (rest lista))))))

(Analiza '(a () 2 3 (a b c))) ; (A N A A L)

;10)
(defun Suma-numerica (lista resultado)
       (cond ((null lista) resultado)
       	     ((numberp (first lista)) (Suma-numerica (rest lista) (+ resultado (first lista))))
		         (T (Suma-numerica (rest lista) resultado))))

 (SUMA-numerica '(1 2 3 a b 2 c 3) 0) ; 11

;11)
(defun FiltraVocales (l)
  (cond ((null l) nil)
        ((atom (car l)) (if (or (equal 'a (car l)) (equal 'e (car l)) (equal 'i (car l)) (equal 'o (car l)) (equal 'u (car l)))
			     (FiltraVocales (cdr l)) (cons (car l) (Filtravocales (cdr l)))))
        (t (append (FiltraVocales (car l)) (FiltraVocales (cdr l))))))

;12)
(defun Filtra-multiplos (lista elem)
       (cond ((null lista) nil)
       	     ((not (equal 0 (mod (first lista) elem))) (cons (first lista) (Filtra-multiplos (rest lista) elem)))
		         (T (Filtra-multiplos (rest lista) elem))))

(filtra-multiplos '(1 2 3 4 5 6) 2) ; (1 3 5)

;13)
(defun Celdas (lista cont)
       (cond ((null lista) cont)
       	     ((atom (first lista)) (Celdas (rest lista) (+ cont 1)))
		         (T (+ (Celdas (first lista) cont) (Celdas (rest lista) (+ cont 1))))))

(Celdas '(((1)) 2 3 4 5) 0) ; 7

;14)
(defun Implica (&rest a)
  (cond ((null a) t)
       	(t (AND (car a) (Implica (cdr a))))))

(Implica T T nil t) ; Nil

;15)
(defun matrix-multiply (m1 m2)
 (mapcar
  (lambda (row)
   (apply #'mapcar
    (lambda (&rest column)
      (apply #'+ (mapcar #'* row column))) m2)) m1))

(matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4))) ; ((-7 -6 11) (-17 -20 25))

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

;18)
(defun fib (n)
  "Tail-recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000005 seconds of total run time (0.000005 user, 0.000000 system)
;100.00% CPU
;8,616 processor cycles
;0 bytes consed

(defun fib (n)
  "loop-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000005 seconds of total run time (0.000005 user, 0.000000 system)
;100.00% CPU
;7,012 processor cycles
;0 bytes consed

(defun fib (n)
  "do-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000004 seconds of total run time (0.000003 user, 0.000001 system)
;100.00% CPU
;5,504 processor cycles
;0 bytes consed

(defun fib (n)
  "CPS computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n k)
                    (if (zerop n)
                        (funcall k 0 1)
                      (fib-aux (1- n) (lambda (x y)
                                        (funcall k y (+ x y)))))))
          (fib-aux n #'(lambda (a b) a))))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000006 seconds of total run time (0.000006 user, 0.000000 system)
;100.00% CPU
;7,788 processor cycles
;4,088 bytes consed

(defun fib (n)
   (labels ((fib2 (n)
                 (cond ((= n 0)
                        (values 1 0))
                       (t
                        (multiple-value-bind (val prev-val)
                                             (fib2 (- n 1))
                           (values (+ val prev-val)
                                   val))))))
      (nth-value 0 (fib2 n))))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000004 seconds of total run time (0.000004 user, 0.000000 system)
;100.00% CPU
;6,634 processor cycles
;0 bytes consed

(defun fib (n)
  "Successive squaring method from SICP"
  (check-type n (integer 0 *))
  (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                          ((evenp count)
                           (fib-aux a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
                          (t (fib-aux (+ (* b q) (* a q) (* a p))
                                      (+ (* b p) (* a q))
                                      p
                                      q
                                      (- count 1))))))
          (fib-aux 1 0 0 1 n)))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000005 seconds of total run time (0.000005 user, 0.000000 system)
;100.00% CPU
;7,900 processor cycles
;0 bytes consed

(defun fib (n)
  (if (< n 2) n
    (if (oddp n)
      (let ((k (/ (1+ n) 2)))
        (+ (expt (fib k) 2) (expt (fib (1- k)) 2)))
      (let* ((k (/ n 2)) (fk (fib k)))
        (* (+ (* 2 (fib (1- k))) fk) fk)))))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000010 seconds of total run time (0.000009 user, 0.000001 system)
;100.00% CPU
;18,438 processor cycles
;0 bytes consed

;; Taken from Winston's Lisp, 3rd edition, this is a tail-recursive version, w/o an auxiliary function
(defun fib (n &optional (i 1) (previous-month 0) (this-month 1))
 (if (<= n i)
      this-month
    (fib n (+ 1 i) this-month (+ this-month previous-month))))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000004 seconds of total run time (0.000004 user, 0.000000 system)
;100.00% CPU
;6,628 processor cycles
;0 bytes conse

;; Fibonacci - Binet's Formula
(defun fib (n)
  (* (/ 1 (sqrt 5))
     (- (expt (/ (+ 1 (sqrt 5)) 2) n)
	(expt (/ (- 1 (sqrt 5)) 2) n))))

(time (fib 50))
;Evaluation took:
;0.048 seconds of real time
;0.000985 seconds of total run time (0.000048 user, 0.000937 system)
;2.08% CPU
;119,580,276 processor cycles
;14 page faults
;0 bytes consed

(defun fib (n)
  (/ (- (expt (/ (+ 1 (sqrt 5)) 2) n)
        (expt (/ (- 1 (sqrt 5)) 2) n))
     (sqrt 5)))

(time (fib 50))
;Evaluation took:
;0.000 seconds of real time
;0.000010 seconds of total run time (0.000009 user, 0.000001 system)
;100.00% CPU
;16,166 processor cycles
;4,096 bytes consed

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

;23)
(defun qsort (l)
   (cond
   ((null l) nil)
   (t (append
      (qsort (listMenor (car l) (cdr l)))
      (cons (car l) nil)
      (qsort (listMayor (car l) (cdr l)))))))

(defun listMenor (a b)
    (cond
    (( or (null a) (null b)) nil)
    (( < a (car b)) (listMenor a (cdr b)))
    (t (cons (car b) (listMenor a (cdr b))))))

(defun listMayor (a b)
    (cond
    (( or ( null a)(null b)) nil)
    (( >= a (car b)) (listMayor a (cdr b)))
    (t (cons (car b) (listMayor a (cdr b))))))

(qsort '(6 5 4 3 1)) ; (1 3 4 5 6)
