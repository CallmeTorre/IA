(defparameter *board* '(()(A V R)(A V R)(A V R)(A V R)(A V R)(A V R)
                       (A V R)(A V R)(A V R)(A V R)(A V R)(A V R)()))
(defparameter *shoot-again* T)
(defparameter *slots-shooted* nil)
(defparameter *id* 0)
(defparameter *infinite* most-positive-fixnum)
(defparameter *current-ancestor* nil)
(defparameter *ops* '((:primera-casilla 6)
                      (:segunda-casilla 5)
                      (:tercera-casilla 4)
                      (:cuarta-casilla 3)
                      (:quinta-casilla 2)
                      (:sexta-casilla 1)))
(defparameter *previous-slots* nil)
(defparameter *end-game* nil)
(defparameter *winner-player* nil)
(defparameter *IA-points* 0)
(defparameter *human-points* 0)

(defun count-pieces (casilla A V R)
  (cond ((null casilla) (+ (* A 1) (* V 5) (* R 10)))
        ((equal (first casilla) 'A) (count-pieces (rest casilla) (1+ A) V R))
        ((equal (first casilla) 'V) (count-pieces (rest casilla) A (1+ V) R))
        ((equal (first casilla) 'R) (count-pieces (rest casilla) A V (1+ R)))
        (T (count-pieces (rest casilla) A V R))))

;(defun order-balls (casilla aux)
;  (cond ((null casilla) aux)
;        ((equal (first casilla) 'R) (order-balls (rest casilla) (push 10  aux)))
;        ((equal (first casilla) 'V) (order-balls (rest casilla) (push 5 aux)))
;        ((equal (first casilla) 'A) (order-balls (rest casilla) (push 1 aux)))
;        (T (order-balls (rest casilla) aux))))

;(defun order-again-balls (casilla aux)
;  (cond ((null casilla) aux)
;        ((= (first casilla) 10) (order-again-balls (rest casilla) (push 'R  aux)))
;        ((= (first casilla) 5) (order-again-balls (rest casilla) (push 'V aux)))
;        ((= (first casilla) 1) (order-again-balls (rest casilla) (push 'A aux)))
;        (T (order-again-balls (rest casilla) aux))))

(defun print-board ()
  (print "///////////////////////////////////////////////////")
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (count-pieces (nth 0 *board*) 0 0 0)(nth 1 *board*)(nth 2 *board*)(nth 3 *board*)(nth 4 *board*)(nth 5 *board*)(nth 6 *board*))
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A"
           (nth 7 *board*)(nth 8 *board*)(nth 9 *board*)(nth 10 *board*)(nth 11 *board*)(nth 12 *board*)(count-pieces (nth 13 *board*) 0 0 0))
  (print "///////////////////////////////////////////////////"))

(defun reset-game ()
  (setq *board* '(()(A V R)(A V R)(A V R)(A V R)(A V R)(A V R)
                  (A V R)(A V R)(A V R)(A V R)(A V R)(A V R)())))

(defun game-ended? ()
  (return-from game-ended? (or (and
                                (null (nth 0 *board*))
                                (null (nth 1 *board*))
                                (null (nth 2 *board*))
                                (null (nth 3 *board*))
                                (null (nth 4 *board*))
                                (null (nth 5 *board*)))
                               (and
                                (null (nth 7 *board*))
                                (null (nth 8 *board*))
                                (null (nth 9 *board*))
                                (null (nth 10 *board*))
                                (null (nth 11 *board*))
                                (null (nth 12 *board*))))))

(defun get-balls (casilla)
  (nth casilla *board*))

(defun same-slot? (casilla-actual)
 ( member casilla-actual *slots-shooted*))

;(defun move-ball (casilla-actual casilla-target)
;  (let* ((canica (pop (nth casilla-actual *board*))))
;    (format t "~& Canica ~A moviendo a ~A ~%" canica (nth casilla-target *board*))
;    (push canica (nth casilla-target *board*))
;    (format t "~& Casilla escogida: ~A ~%" casilla-actual)
;    (format t "~& Casilla target: ~A ~%" casilla-target)
;    (print-board)))

(defun insert-ball (lista)
  (loop for x in lista do
       (push (first x) (nth (second x) *board*))
       (push (second x) *slots-shooted*)))

(defun move-ball (casilla-actual)
  (let ((canicas (get-balls casilla-actual))
        (movimiento nil))
    (format t "~& Canicas en casilla:  ~A ~%" canicas)
    (format t "~& Indique la casilla a la cual ira cada canica de la forma ((canica casilla)(canica casilla)...)~%")
    (setq movimiento(read))
    (insert-ball movimiento)
    (loop for canica in canicas do
         (pop (nth casilla-actual *board*)))
    (print-board)))

(defun valid-human-slot? (casilla-actual)
  (cond ((member casilla-actual '(1 2 3 4 5 6)) nil)
        ((null (get-balls casilla-actual)) nil)
        (T T)))

(defun human-turn()
  (let* ((casilla-actual nil)
         (mValido nil))
    (print-board)
    (loop until (null *shoot-again*) do
         (loop until mValido do
              (print "~& Escoge una casilla ~%")
              (print "~& #ProTip: Solo puedes escoger las casillas de la parte de abajo del tablero (7,8,9,10,11,12)")
              (print "~& ProTip2:A = 1, V  = 5, R = 10")
              (if (valid-human-slot? (setq casilla-actual(read)))
                  (setq mValido T)
                  (print "Tu movimiento no es valido")))
         (move-ball casilla-actual)
         (if (= (first *slots-shooted*) 13)
             (setq *shoot-again* T)
             (setq *shoot-again* nil))
         (setq casilla-actual nil
               *slots-shooted* nil
               mValido nil)
         (if (game-ended?)
             (progn
               (setq *end-game* (game-ended?)
                     *winner-player* 0
                     *shoot-again* nil))))))

;(defun human-turn ()
;  (let* ((casilla-actual nil)
;         (canicas-casilla nil)
;         (mValido nil)
;         (casilla-target nil))
;    (loop until (null *shoot-again*) do
;         (loop until mValido do
 ;             (print-board)
  ;            (print "~& Escoge una casilla ~%")
  ;            (print "~& #ProTip: Solo puedes escoger las casillas de la parte de abajo del tablero (7,8,9,10,11,12)")
  ;            (print "~& ProTip2:A = 1, V  = 5, R = 10")
  ;            (if (valid-human-slot? (setq casilla-actual(read)))
  ;                (progn
  ;                  (setq canicas-casilla (get-balls casilla-actual))
  ;                  (setq mValido T))
   ;               (print "Tu movimento no es valido")))
    ;     (loop for canica in canicas-casilla do
    ;          (format t "~& ¿En que casilla quieres colocar ~A ~%?" canica)
     ;         (loop until (null (same-slot? (setq casilla-target(read)))) do
      ;             (print "~& Ya tiraste en esa casilla"))
       ;       (push casilla-target *slots-shooted*)
        ;      (move-ball casilla-actual casilla-target))
        ; (if (= (first *slots-shooted*) 13)
        ;     (setq *shoot-again* T)
        ;     (setq *shoot-again* nil))
        ; (setq casilla-actual nil
        ;       casilla-target nil
        ;       *slots-shooted* nil
         ;      mValido nil
         ;      canicas-casilla nil)
        ; (if (game-ended?)
         ;    (progn
         ;      (setq *end-game* (game-ended?))
         ;      (setq *winner-player* 0)
         ;      (setq *shoot-again* nil))))))

;///////////////////////////////////////////////////
;Human Stuff
;///////////////////////////////////////////////////

(defun valid-operator? (operador estado)
  (let ((operador (second operador)))
    (cond ((= operador 6)
           (if (null (nth 6 estado)) nil T))
          ((= operador 5)
           (if (null (nth 5 estado)) nil T))
          ((= operador 4)
           (if (null (nth 4 estado)) nil T))
          ((= operador 3)
           (if (null (nth 3 estado)) nil T))
          ((= operador 2)
           (if (null (nth 2 estado)) nil T))
          ((= operador 1)
           (if (null (nth 1 estado)) nil T))
          (T nil))))

(defun apply-operator (operador estado)
  (let* ((ops (first operador))
         (casilla-actual (second operador))
         (canicas-casilla (get-balls casilla-actual))
         (estado-resultado nil))
    (case ops
      (:primera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:segunda-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:tercera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:cuarta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:quinta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:sexta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (T "Error"))
    estado-resultado))

(defun move-machine-balls (estado casilla canicas)
  (let* ((estado-copia (copy-list estado))
         (canica-copia nil)
         (longitud-canicas (length canicas))
         (shoot-again nil)
         (best-canica nil)
        ; (canicas-aux nil)
        ; (canicas-aux2 nil)
         (pos-best-canica nil)
         (casilla-target (1- casilla))
         (cont 0))

    (cond ((null (position 'R canicas :test #'equal))(setq pos-best-canica (position 'V canicas :test #'equal)))
          ((and (null (position 'R canicas :test #'equal))(null (position 'V canicas :test #'equal)))(setq pos-best-canica (position 'A canicas :test #'equal)))
          (T (setq pos-best-canica (position 'R canicas :test #'equal))))

    (if (>= longitud-canicas casilla)
        (progn
          (setq best-canica (nth pos-best-canica canicas))
          (push best-canica (nth 0 estado-copia))))

    ;(setq canicas-aux (order-balls canicas ()))
    ;(setq canicas-aux2 (sort canicas-aux #'<))
    ;(setq canicas (order-again-balls canicas-aux2 ()))

    (if (= 0 (- longitud-canicas casilla))
        (setq shoot-again T)
        (setq shoot-again nil))

    (loop for canica in canicas do
         (setq canica-copia (pop (nth casilla estado-copia)))
         (if (and (= cont 0)(equal best-canica canica-copia))
               (setq cont (1+ cont))
             (progn
               (if (< casilla-target 0)
                   (progn
                     (setq casilla-target 7)
                     (push canica-copia (nth casilla-target estado-copia))
                     (setq casilla-target (1+ casilla-target)))
                   (progn
                     (push canica-copia (nth casilla-target estado-copia))
                     (setq casilla-target (1- casilla-target))))
               (if (> casilla-target 13)
                   (progn
                     (setq casilla-target 6)
                     (push canica-copia (nth casilla-target estado-copia))
                     (setq casilla-target (1- casilla-target))))))
         finally (return (list estado-copia shoot-again casilla)))))

(defun heuristic-function (estado)
  (let ((movimiento 0)
        (tablero (first estado)))
    (setq movimiento
          (+ (- (count-pieces (nth 0 tablero) 0 0 0)(count-pieces (nth 13 tablero) 0 0 0))
             (- (+ (count-pieces (nth 1 tablero) 0 0 0)(count-pieces (nth 2 tablero) 0 0 0)(count-pieces (nth 3 tablero) 0 0 0)
                   (count-pieces (nth 4 tablero) 0 0 0)(count-pieces (nth 5 tablero) 0 0 0)(count-pieces (nth 6 tablero) 0 0 0))
                (+ (count-pieces (nth 7 tablero) 0 0 0)(count-pieces (nth 8 tablero) 0 0 0)(count-pieces (nth 9 tablero) 0 0 0)
                   (count-pieces (nth 10 tablero) 0 0 0)(count-pieces (nth 11 tablero) 0 0 0)(count-pieces (nth 12 tablero) 0 0 0)))))
    (cond ((not (null (second estado)))
           (setq movimiento -10000))
          ((and (or (null (nth 1 tablero))(null (nth 2 tablero))(null (nth 3 tablero)))
                (or (> (length (nth 6 tablero)) 3)(> (length (nth 5 tablero)) 3)))
           (setq movimiento 0))
          (T movimiento))
    movimiento))

(defun change-player (jugador)
  (case jugador
    (0 1)
    (1 0)))

(defun expand (estado)
  (let* ((sucesores nil)
         (nuevo-estado nil)
         (estado-copia (copy-list estado)))
    (loop for operador in *ops* do
         (if (valid-operator? operador estado-copia)
             (progn
               (setq nuevo-estado (apply-operator operador estado-copia))
               (push nuevo-estado sucesores)))
       finally (return sucesores))))

(defun minimax (estado profundidad max-profundidad jugador infinito menos-infinito)
  (if (= profundidad max-profundidad)
      (heuristic-function estado)
      (let ((sucesores (expand estado)))
        (if (null sucesores)
            (heuristic-function estado)
            (do ((nuevo-valor nil)
                 (mejor-mov (first sucesores)))
                ((null sucesores)
                 (if (= profundidad 0)
                     mejor-mov
                     menos-infinito))
              (setf nuevo-valor
                    (- (minimax
                        (first sucesores)
                        (1+ profundidad)
                        max-profundidad
                        (change-player jugador)
                        (- menos-infinito)
                        (- infinito))))
              (when (> nuevo-valor menos-infinito)
                (setf menos-infinito nuevo-valor)
                (setf mejor-mov (first sucesores)))
              (if (>= infinito menos-infinito)
                  (setf sucesores nil)
                  (setf sucesores (rest sucesores))))))))

(defun machine-turn ()
  (let ((movimiento nil)
        (movimiento-final nil)
        (shoot-again T))
    (loop until (null shoot-again) do
         (setq movimiento (minimax *board* 0 1 1 *infinite* (- *infinite*)))
         (setq movimiento-final (first movimiento))
         (setq shoot-again (second movimiento))
         (format t "~& La casilla que escogio la IA fue ~A ~%" (third movimiento))
         (setq *board* movimiento-final))
    (if (game-ended?)
        (progn
          (setq *end-game* (game-ended?))
          (setq *winner-player* 0)
          (setq *shoot-again* nil)))
    (print-board)))

(defun play ()
  (reset-game)
  (loop until (not (null *end-game*)) do
       (game-ended?)
       (human-turn)
       (setq *shoot-again* T)
       (if (null *end-game*)
           (machine-turn)))
  (if (= *winner-player* 1)
      (setq *IA-points*
            (+ (count-pieces (nth 7 *board*) 0 0 0)
               (count-pieces (nth 8 *board*) 0 0 0)
               (count-pieces (nth 9 *board*) 0 0 0)
               (count-pieces (nth 10 *board*) 0 0 0)
               (count-pieces (nth 11 *board*) 0 0 0)
               (count-pieces (nth 12 *board*) 0 0 0)
               (count-pieces (nth 0 *board*) 0 0 0))))
  (if (= *winner-player* 0)
      (setq *human-points*
            (+ (count-pieces (nth 1 *board*) 0 0 0)
               (count-pieces (nth 2 *board*) 0 0 0)
               (count-pieces (nth 3 *board*) 0 0 0)
               (count-pieces (nth 4 *board*) 0 0 0)
               (count-pieces (nth 5 *board*) 0 0 0)
               (count-pieces (nth 6 *board*) 0 0 0)
               (count-pieces (nth 13 *board*) 0 0 0))))
  (format t "~& Tu puntuación: ~A Puntuacion de la IA: ~A ~%" *human-points* *IA-points*))

;(trace machine-turn)
;(trace minimax)
;(trace expand)
;(trace apply-operator)
;(trace move-machine-balls)
;(trace heuristic-function)
(play)
