(defparameter *board* '(()(A V R)(A V R)(A V R)(A V R)(A V R)(A V R)
                       (A V R)(A V R)(A V R)(A V R)(A V R)(A V R)()))
(defparameter *shoot-again* nil)
(defparameter *id* 0)
(defparameter *current-ancestor* nil)
(defparameter *ops* '((:primera-casilla 7)
                      (:segunda-casilla 8)
                      (:tercera-casilla 9)
                      (:cuarta-casilla 10)
                      (:quinta-casilla 11)
                      (:sexta-casilla 12)))
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
  (let* ((casilla-target nil)
         (canicas-casilla nil)
         (moviento-valido nil)
         (casilla-target))
    (loop until (null *shoot-again*) do
         (loop until movimiento-valido do
              (if (valid-human-slot? (setq casilla-actual (read)))
                  (progn
                    (setq canicas-casilla (get-balls casilla-actual))
                    (setq movimiento-valido T))
                  (print "Tu movimento no es valido")))
        ;//Terminar el loop para cada canica de tu slot ))

