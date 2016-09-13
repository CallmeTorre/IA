;Jesús Alexis Torreblanca Faces

;1)
(defun Collect (fun l)
    (cond ((null lista) nil)
	   (t (Collect (funcall fun (car lista) (cadr lista))))))

(Collect #'+ '(1 2 3 4)) ; 10

;2)
(defun palindromo (lista)
       (if (equal nil lista) T
       	   (if (equal (first lista) (first (last lista))) (palindromo (rest (butlast lista))) nil)))

(palindromo '(a b a)) ; T

;3)
(defun 2palindrome (cadena &optional (inicio 0))
       (if (= (length cadena) 0) T
       	    (if (char= (char cadena inicio) (char (reverse cadena) inicio))
      	    	(2palindrome (subseq cadena (+ inicio 1) (- (length cadena) 1)) inicio) nil)))

(2palindrome "abba") ; T

;4)
(defun iterativepalindrome (lista)
       (loop for i from 0
       	     for a in lista
	           for b in (reverse lista)
	           always (equal a b)
	           until (> i(/(length lista)2))))

(iterativepalindrome '(a b a)) ; T

;5)
(defun listRotate (l &key (right nil) (left nil))
  (if right
      (rotate l right)
      (rotate l (* -1 left))))

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))

(listRotate '(a b c d e f g h) :right 3) ; (D E F G H A B C)

;6)
(defun Max&Pos (a)
       (let* ((dim (array-dimensions a))
       (f (first dim))
       (c (second dim))
       (elem 0)
       (pos 0))
       (loop for i from 0 to (- c 1)
            collect(dotimes(j f (cons elem pos))
			 	               (if (< elem (aref a j i)) (setq pos j))
					                  (setq elem (max elem (aref a j i)))))))

(setq mat (make-array '(3 3) :initial-contents
'((1 2 3)
  (4 5 6)
  (7 8 9))))
(Max&Pos mat) ; ((7 . 2) (8 . 2) (9 . 2))

;7)
(defun Combine (fun lista)
       (cond ((null lista) 0)
       	     (T (funcall fun (first lista) (second lista) (Combine fun (cddr lista))))))

;8)
(defun levelAux (cadena lista cont)
       (cond ((null lista) cont)
       	     	((listp (first lista)) (levelAux cadena (first lista) (1+ cont)))
		          ((equal cadena (string(first lista))) (return-from levelAux cont))
       		    (T (levelAux cadena (rest lista) cont))))
(levelAux "hola" ((hola a ) b c) 0)

;9)
(defun encode (list)
  (labels ((encode-run (element count list)
             (cond
               ((null list) (list (list count element)))
               ((eql element (first list)) (encode-run element (1+ count) (rest list)))
               (t (cons (list count element) (encode-run (first list) 1 (rest list)))))))
    (if (null list)
        '()
        (encode-run (first list) 1 (rest list)))))

(encode '(a a a a b c c a a d e e e e)) ; ((4 A) (1 B) (2 C) (2 A) (1 D) (4 E))

;10)
(defun StrCypherAux (cadena code ct)
  (cond ((equal (length cadena) ct) (return-from StrCypherAux cadena))
	(t (StrCypherAux (substitute (char code ct) (char cadena ct) cadena) code (+ ct 1)))))

(defun StrCypher (c cc)
  (StrCypherAux c cc 0))

(StrCypher "Clor" "abcd") ; "abcd"

;11)
(defun mmul (A B)
  (if (not (equal (car (array-dimensions A)) (car (array-dimensions B))))
      (return-from mmul nil))
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
              (loop for k from 0 to (- l 1) do
                    (setf (aref C i k)
                          (loop for j from 0 to (- n 1)
                                sum (* (aref A i j)
                                       (aref B j k))))))
    C))

(mmul #2a((1 2) (3 4)) #2a((5 6 7) (8 9 10))) ; #2A((21 24 27) (47 54 61))

(length #2a((1 2) (3 4)))
(equal (array-dimensions #2a((1 2) (3 4))) (array-dimensions #2a((1 2) (1 2))))
(car (array-dimensions #2a((1 2) (3 4))))
(car (array-dimensions #2a((-3 -8 3) (-2 1 4))))

;13)
(defun recorta (l n)
  (cond ((equal 0 n) nil)
	(t (cons (car l) (recorta (cdr l) (- n 1))))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun filtersubset (lista posicion)
  (all-permutations ( recorta lista posicion)))

(filtersubset '(a b c d) 2) ; ((A B) (B A))

;14)
(defun combinations (count list)
  (cond
    ((zerop count) '(()))
    ((endp list) '())
    (t (nconc (mapcar (let ((item (first list))) (lambda (combi) (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))

(combinations 5 '(a b c d e f)); ((A B C D E) (A B C D F) (A B C E F) (A B D E F) (A C D E F) (B C D E F))

;15)
(defmacro If-positive (number)
  (let ((c (gensym "cond-")))
    `(let* ((,c ,number))
       (if (plusp ,c)
	   (print "NumeroPositivo")
	   (print "NumeroNegativo")))))

(If-positive 10) ; Numero Positivo 
