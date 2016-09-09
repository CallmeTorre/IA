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
