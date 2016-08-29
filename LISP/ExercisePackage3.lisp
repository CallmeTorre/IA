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
