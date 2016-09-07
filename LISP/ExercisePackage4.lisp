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
