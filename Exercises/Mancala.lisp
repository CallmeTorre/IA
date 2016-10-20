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

(defun print-board ()
  (print "///////////////////////////////////////////////////")
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A~%"
           (nth 0 *board*)(nth 1 *board*)(nth 2 *board*)(nth 3 *board*)(nth 4 *board*)(nth 5 *board*)(nth 6 *board*))
  (format  t  "~& ~A ~A ~A ~A ~A ~A ~A"
           (nth 7 *board*)(nth 8 *board*)(nth 9 *board*)(nth 10 *board*)(nth 11 *board*)(nth 12 *board*)(nth 13 *board*))
  (print "///////////////////////////////////////////////////"))

(defun reset-game ()
  (setq *board* '(()(A V R)(A V R)(A V R)(A V R)(A V R)(A V R)
                  (A V R)(A V R)(A V R)(A V R)(A V R)(A V R)())))

(defun get-balls (casilla)
  (nth casilla *board*))

(defun same-slot? (casilla-actual)
 ( member casilla-actual *slots-shooted*))

(defun move-ball (casilla-actual casilla-target)
  (let* ((canica (pop (nth casilla-actual *board*))))
    (format t "~& Canica ~A moviendo a ~A ~%" canica (nth casilla-target *board*))
    (push canica (nth casilla-target *board*))
    (format t "~& Casilla escogida: ~A ~%" casilla-actual)
    (format t "~& Casilla target: ~A ~%" casilla-target)
    (print-board)))

(defun valid-human-slot? (casilla-actual)
  (cond ((member casilla-actual '(1 2 3 4 5 6)) nil)
        ((null (get-balls casilla-actual)) nil)
        (T T)))

(defun human-turn ()
  (let* ((casilla-actual nil)
         (canicas-casilla nil)
         (mValido nil)
         (casilla-target nil))
    (loop until (null *shoot-again*) do
         (loop until mValido do
              (print-board)
              (print "~& Escoge una casilla ~%")
              (print "~& #ProTip: Solo puedes escoger las casillas de la parte de abajo del tablero (7,8,9,10,11,12)")
              (print "~& ProTip2:A = 1, V  = 5, R = 10")
              (if (valid-human-slot? (setq casilla-actual(read)))
                  (progn
                    (setq canicas-casilla (get-balls casilla-actual))
                    (setq mValido T))
                  (print "Tu movimento no es valido")))
         (loop for canica in canicas-casilla do
              (format t "~& ¿En que casilla quieres colocar ~A ~%?" canica)
              (loop until (null (same-slot? (setq casilla-target(read)))) do
                   (print "~& Ya tiraste en esa casilla"))
              (push casilla-target *slots-shooted*)
              (move-ball casilla-actual casilla-target))
         (if (= (first *slots-shooted*) 13)
             (setq *shoot-again* T)
             (setq *shoot-again* nil))
         (setq casilla-actual nil
               casilla-target nil
               *slots-shooted* nil
               mValido nil
               canicas-casilla nil))))

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
    (print ops)
    (case ops
      (:primera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:segunda-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:tercera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:cuarta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:quinta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:sexta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (T "Error"))
    estado-resultado))

;[Check]
(defun move-machine-balls (estado casilla canicas)
  (let* ((estado-copia (copy-list estado))
         (canica-copia nil)
         (casilla-target (1- casilla))
         (cont 0))
    (loop for canica in canicas do
         (setq canica-copia (pop (nth casilla estado-copia)))
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
               (setq casilla-target (1- casilla-target))))
         finally (return (list estado-copia)))))

(defun count-pieces (casilla A V R)
  (cond ((null casilla) (+ A V R))
        ((equal (first casilla) 'A) (count-pieces (rest casilla) (1+ A) V R))
        ((equal (first casilla) 'V) (count-pieces (rest casilla) A (1+ V) R))
        ((equal (first casilla) 'R) (count-pieces (rest casilla) A V (1+ R)))
        (T (count-pieces (rest casilla) A V R))))

(defun heuristic-function (estado)
  (+ (- (count-pieces (nth 0 estado) 0 0 0)(count-pieces (nth 13 estado) 0 0 0))
     (- (+ (count-pieces (nth 1 estado) 0 0 0)(count-pieces (nth 2 estado) 0 0 0)(count-pieces (nth 3 estado) 0 0 0)
           (count-pieces (nth 4 estado) 0 0 0)(count-pieces (nth 5 estado) 0 0 0)(count-pieces (nth 6 estado) 0 0 0))
        (+ (count-pieces (nth 7 estado) 0 0 0)(count-pieces (nth 8 estado) 0 0 0)(count-pieces (nth 9 estado) 0 0 0)
           (count-pieces (nth 10 estado) 0 0 0)(count-pieces (nth 11 estado) 0 0 0)(count-pieces (nth 12 estado) 0 0 0)))))

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
            (heuristic-function (first estado))
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
        (movimiento-final nil))
    (loop until (null *shoot-again*) do
         (setq movimiento (minimax *board* 0 1 1 *infinite* (- *infinite*)))
         (setq movimiento-final (first movimiento))
         (setq *board* movimiento-final)
         (setq *shoot-again* nil))
    (print-board)))

(defun play ())

