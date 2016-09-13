;Jesús Alexis Torreblanca Faces

;1.a) El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))) sin usar la función FIFTH
(first (rest (rest (rest (rest '(((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))))))) ;(B C)

;1.b) El numero de segundos que tiene el año bisiesto 2004
(* 366 24 3600) ;31622400

;1.c) Si el valor numerico asociado a la variable x es diferente de cero y además menor o igual que el valor asociado a la variable y.
(setq x 10) ;x=10
(setq y 20) ;y=20

;1.d) Una lista con las dos soluciones reales de la ecuación 2x^2+7x+5=0
(list (/(+ 7(sqrt (-(expt 7 2)(* 4 2 5))))(* 2 2)) (/(- 7(sqrt (-(expt 7 2)(* 4 2 5))))(* 2 2)));(2.5 1.0)

;2.a)
(+ (* 2 4)(- 6 8)) ;6

;2.b)
(/ (+ 5(+ -3 4))(+ 6 .4)) ;.9375

;2.c)
(sqrt (/ (+ (-(- -4 3/8)) 1.4502)(expt -1 (expt -2 1/3)))) ;#C(7.355944 -11.196843)

;2.d)
(expt (/ (expt (/ 65.402 (sqrt -1)) 1/5) .17) 1/7) ;#C(1.4500145 -0.065120235)


;3.a) (cdar '((one two) three four)))
(TWO)

;3.b) (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
((EVA LISA) KARL SVEN EVA LISA KARL SVEN)

;3.c) (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
(EVA GITAN LISA GITAN KARIN)

;3.d) (remove 'sven '(eva sven lisa sven anna))
(EVA LISA ANNA)

;3.e) (butlast '(karl adam nilsson gregg alisson vilma) 3)
(KARL ADAM NILSSON)

;3.f) (nth 2 '(a b c d e))
C

;3.g) (nthcdr 2 '(a b c d e))
(C D E)

;3.h) (intersection '(a b c) '(x b z c))
(C B)

;3.i) (cdadar '(((((1 2 3) z) y)(x 4)) 7 8 (a b c (5 (6 7 8)))))
(4)

;4)
(defun Recombina (lista)
       (let ((A (first (first lista)))
       	    (X (rest (first lista)))
	          (B (first(first (rest lista))))
	          (Y (rest(second lista)))
	          (C (first (third lista)))
	          (Z (rest (third lista))))
	     (list (cons (list X Y) A) (cons (list Y Z) C) (cons (list Z Y X) B))))

(RECOMBINA '((A . x)(B . Y)(C . Z))) ;(((X Y) . A) ((Y Z) . C) ((Z Y X) . B))

;5)
(defun NoCero? (n)
       (if (or (plusp n)(minusp n)) "No es Cero" "Es cero"))

(NoCero? 10) ;No es Cero
(NoCero? -10) ;No es Cero
(NoCero? 0) ;Es cero

;6)
(defun Analiza (x)
     (list (if (atom x) "Es un atomo" "No es un atomo")
     (if (numberpx) "Es un numero" "No es un numero")
     (if (listp x) "Es una lista" "No es una lista")
     (if (consp x) "Es una celda de construcción" "No es una celda de construcción")
     (if (null x) "Es nulo" "No es nulo")))

(ANALIZA 1) ;("Es un atomo" "Es un numero" "No es una lista" "No es una celda de construcción" "No es nulo")
