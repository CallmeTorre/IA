(defparameter *board* '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()))
(defparameter *shoot-again* T)
(defparameter *slots-shooted* nil)
(defparameter *id* 0)
(defparameter *infinite* most-positive-fixnum)
(defparameter *current-ancestor* nil)
(defparameter *ops* '((:primera-casilla 7)
                      (:segunda-casilla 8)
                      (:tercera-casilla 9)
                      (:cuarta-casilla 10)
                      (:quinta-casilla 11)
                      (:sexta-casilla 12)))
(defparameter *previous-slots* nil)
(defparameter *end-game* nil)
(defparameter *winner-player* nil)
(defparameter *IA-points* 0)
(defparameter *human-points* 0)

(defun print-board ()
  (format  t   "~&~% | ~A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%"
           (apply #'+ (nth 13 *board*))(apply #'+ (nth 12 *board*)) (apply #'+ (nth 11 *board*)) (apply #'+ (nth 10 *board*)) (apply #'+ (nth 9 *board*)) (apply #'+ (nth 8 *board*)) (apply #'+ (nth 7 *board*)))
  (format  t   "~& | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%~%"
           (apply #'+ (nth 0 *board*)) (apply #'+ (nth 1 *board*)) (apply #'+ (nth 2 *board*)) (apply #'+ (nth 3 *board*)) (apply #'+ (nth 4 *board*)) (apply #'+ (nth 5 *board*)) (apply #'+ (nth 6 *board*))))

(defun reset-game ()
  (setq *board* '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)())))

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

(defun insert-ball (lista casilla)
  (let ((casilla-siguiente (1+ casilla)))
    (loop for x in lista do
         (push x (nth casilla-siguiente *board*))
         (push casilla-siguiente *slots-shooted*)
         (setq casilla-siguiente (1+ casilla-siguiente))
         (if (> casilla-siguiente 13)
             (setq casilla-siguiente 0)
             ))))

(defun move-ball (casilla-actual)
  (let ((canicas (get-balls casilla-actual))
        (movimiento nil))
    (format t "~& Canicas en casilla:  ~A ~%" canicas)
    (format t "~& Indique la casilla a la cual ira cada canica de la forma (canica canica canica ...)~%")
    (setq movimiento(read))
    (insert-ball movimiento casilla-actual)
    (loop for canica in canicas do
         (pop (nth casilla-actual *board*)))
    (print-board)))

(defun valid-human-slot? (casilla-actual)
  (cond ((member casilla-actual '(6 7 8 9 10 11 12 13)) nil)
        ((null (get-balls casilla-actual)) nil)
        (T T)))

(defun human-turn()
  (let* ((casilla-actual nil)
         (mValido nil))
    (print-board)
    (loop until (null *shoot-again*) do
         (loop until mValido do
              (print "~& Escoge una casilla ~%")
              (print "~& #ProTip: Solo puedes escoger las casillas de la parte de abajo del tablero (0,1,2,3,4,5)")
              (if (valid-human-slot? (setq casilla-actual(read)))
                  (setq mValido T)
                  (print "Tu movimiento no es valido")))
         (move-ball casilla-actual)
         (if (= (first *slots-shooted*) 6)
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

;///////////////////////////////////////////////////
;Human Stuff
;///////////////////////////////////////////////////

(defun valid-operator? (operador estado)
  (let ((operador (second operador)))
    (cond ((= operador 7)
           (if (null (nth 7 estado)) nil T))
          ((= operador 8)
           (if (null (nth 8 estado)) nil T))
          ((= operador 9)
           (if (null (nth 9 estado)) nil T))
          ((= operador 10)
           (if (null (nth 10 estado)) nil T))
          ((= operador 11)
           (if (null (nth 11 estado)) nil T))
          ((= operador 12)
           (if (null (nth 12 estado)) nil T))
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

(defun move-machine-balls (tablero casillaActual canicasAux)
  (let* ((canicaAux nil)
         (contador 0)
         (estado nil)
         (canicas nil)
         (longitudParaTurno 0)
         (contadorParaTurno 0)
         (copiaTablero nil)
         (canicaMayorEnBase -1)
         (maquinaSeguirTirando nil)
         (casillaAMeter (1+ casillaActual)))

    ;Usamos funciones destructivas por lo que es recomendable trabajar con copias totalmente separadas de lo por
    ; lo que estado es una copia de el estado actual de nuestro tablero
    (setq estado (copy-list tablero))

    ;Por seguridad y para que no guarde referencias a otras celdas de la lista, creamos una copia de los elementos
    ; originales
    (loop for elemento in tablero do
         (setq copiaTablero (cons elemento copiaTablero)))
    (setq copiaTablero  (reverse copiaTablero))
    (loop for can in canicasAux do
         (setq canicas (cons can canicas)))

    ;Realizamos una resta para saber el numero de canicas que le vamos a dar al humano, quedandonos siempre
    ; las de mejor valor asi como insertando la de mayor valor en nuestra base
    (setq canicas(sort canicas #'>))
    (setq longitudParaTurno (length canicas))
    (if ( >= (length canicas) (- 13 casillaActual))
        (progn
          (setq canicaMayorEnBase (first canicas))
          (push canicaMayorEnBase (nth 13 copiaTablero))
                                        ;(setq seguirTirando T)
          ))

    (if ( > (length canicas) (- 13 casillaActual))
        (setq maquinaSeguirTirando nil))

     (if (= 0 (- (length canicas) (- 13 casillaActual)))
         (setq maquinaSeguirTirando T))

     (if (> 0 (- (length canicas) (- 13 casillaActual)))
         (setq maquinaSeguirTirando nil))


    ;Anteriormente ya hemos insertado la canica de mayor valor en nuestra base por lo que detectamos cuando
    ; se repitela canica para no insertarla, asi como debemos reiniciar la cuenta para insertar las otras
    ; canicas en la base enemiga
    (loop for canica in canicas do
         (setq contadorParaTurno (1+ contadorParaTurno))
         (setq canicaAux (pop (nth casillaActual copiaTablero)))
         (if (and ( = contador 0 ) ( = canicaMayorEnBase canicaAux))
             (progn
               (setq contador (1+ contador)))
             (progn
               (if (> casillaAMeter 12)
                   (setq casillaAMeter 0))
               (push canicaAux (nth casillaAMeter copiaTablero))
               (setq casillaAMeter (1+ casillaAMeter))))
       ;  (setq contadorParaTurno (1+ contadorParaTurno))
       finally (return (list copiaTablero maquinaSeguirTirando casillaActual)))))

(defun heuristic-function (estado)
  (let ((movimiento 0)
        (tablero (first estado)))
    (setq movimiento
          (+ (- (apply #'+ (nth 13 tablero)) (apply #'+ (nth 6 tablero)))
             (- (+ (apply #'+ (nth 7 tablero))(apply #'+ (nth 8 tablero))(apply #'+ (nth 9 tablero))
                   (apply #'+ (nth 10 tablero))(apply #'+ (nth 11 tablero))(apply #'+ (nth 12 tablero)))
                (+ (apply #'+ (nth 0 tablero))(apply #'+ (nth 1 tablero))(apply #'+ (nth 2 tablero))
                   (apply #'+ (nth 3 tablero))(apply #'+ (nth 4 tablero))(apply #'+ (nth 5 tablero))))))
    (cond ((not (null (second estado)))
           (setq movimiento -10000))
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

(defun minimax-alpha-beta (tablero profundidad max-profundidad jugador alpha beta)
  ;Si llegamos a la profundidad maxima retornamos la evaluacion de la heuristica
  (if (= profundidad max-profundidad)
      (heuristic-function  tablero)
      ;Si aun no hemos llegado a la profundidad maxima continuamos
      ;Primero recuperamos como sucesores la aplicacion de los operadores al tablero
      (let ((sucesores (expand tablero)))
        ;Si ya no tenemos sucesores que analizar llamamos a la heuristica del Mancala
        ; second de tablero es si puede volver a tirar la maquina o no
        (if (null sucesores)
            (heuristic-function tablero)
            (do ((nuevoValor nil)
                 (mejorMovimiento (car sucesores)))

                ((null sucesores)
                 (if (= profundidad 0)
                     mejorMovimiento
                     beta))

              (setf nuevoValor
                    (- (minimax-alpha-beta
                        (car sucesores)
                        (1+ profundidad)
                        max-profundidad
                        (change-player jugador)
                        (- beta)
                        (- alpha))))
              (when (> nuevoValor beta)
                (setf beta nuevoValor)
                (setf mejorMovimiento (car sucesores)))
              (if (>= beta alpha)
                  (setf sucesores nil)
                  (setf sucesores (cdr sucesores))))))))

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
          (setq *shoot-again* nil)))))

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
            (+ (apply #'+ (nth 0 *board*))
               (apply #'+ (nth 1 *board*))
               (apply #'+ (nth 2 *board*))
               (apply #'+ (nth 3 *board*))
               (apply #'+ (nth 4 *board*))
               (apply #'+ (nth 5 *board*))
               (apply #'+ (nth 13 *board*))))
      (setq *IA-points* (apply #'+ (nth 13 *board*))))
  (if (= *winner-player* 0)
      (setq *human-points*
            (+ (apply #'+ (nth 7 *board*))
               (apply #'+ (nth 8 *board*))
               (apply #'+ (nth 9 *board*))
               (apply #'+ (nth 10 *board*))
               (apply #'+ (nth 11 *board*))
               (apply #'+ (nth 12 *board*))
               (apply #'+ (nth 6 *board*))))
      (setq *human-points* (apply #'+ (nth 6 *board*))))
  (format t "~& Tu puntuación: ~A Puntuacion de la IA: ~A ~%" *human-points* *IA-points*))

;(trace machine-turn)
;(trace minimax)
;(trace expand)
;(trace apply-operator)
;(trace move-machine-balls)
;(trace heuristic-function)
(play)
