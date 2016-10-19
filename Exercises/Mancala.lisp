(defparameter *board* '(()(A V R)(A V R)(A V R)(A V R)(A V R)(A V R)
                       (A V R)(A V R)(A V R)(A V R)(A V R)(A V R)()))
(defparameter *shoot-again* T)
(defparameter *slots-shooted* nil)
(defparameter *id* 0)
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
         (setq casilla-actual nil
               casilla-target nil
               mValido nil
               canicas-casilla nil))))
;///////////////////////////////////////////////////
;Human Stuff
;//////////////////////////////////////////////////

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
  (let* ((operador (first operador))
        (casilla-actual (second operador))
        (canicas-casilla (get-balls casilla-actual))
        (estado-resultado nil))
    (case operador
      (:primera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:segunda-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:tercera-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:cuarta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:quinta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (:sexta-casilla (setq estado-resultado (move-machine-balls estado casilla-actual canicas-casilla)))
      (T "Error"))
    estado-resultado))

(defun move-machine-balls (estado casilla canicas))

(defun heuristic-function (estado))

(defun expand (estado))

(defun machine-turn ())

(defun play ())
