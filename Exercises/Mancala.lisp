;------------------------------------------
;| Jesús Alexis Torreblanca Faces         |
;| Inteligencia Artificial                |
;| Agente Jugador de Mancala              |
;------------------------------------------

(defparameter *board* '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()))
(defparameter *shoot-again* T)
(defparameter *slots-shooted* nil)
(defparameter *alfa* most-positive-fixnum)
(defparameter *ops* '((:primera-casilla 7)
                      (:segunda-casilla 8)
                      (:tercera-casilla 9)
                      (:cuarta-casilla 10)
                      (:quinta-casilla 11)
                      (:sexta-casilla 12)))
(defparameter *end-game* nil)
(defparameter *winner-player* nil)
(defparameter *IA-points* 0)
(defparameter *human-points* 0)

(defun print-board ()
  "Funcion que imprime el tablero del mancala, en cada casilla imprime el valor de la suma de las canicas que se encuentran en esa casilla"
  (format  t   "~&~% | ~A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%"
           (apply #'+ (nth 13 *board*))(apply #'+ (nth 12 *board*)) (apply #'+ (nth 11 *board*)) (apply #'+ (nth 10 *board*)) (apply #'+ (nth 9 *board*)) (apply #'+ (nth 8 *board*)) (apply #'+ (nth 7 *board*)))
  (format  t   "~& | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A |  | ~:A | ~%~%"
           (apply #'+ (nth 0 *board*)) (apply #'+ (nth 1 *board*)) (apply #'+ (nth 2 *board*)) (apply #'+ (nth 3 *board*)) (apply #'+ (nth 4 *board*)) (apply #'+ (nth 5 *board*)) (apply #'+ (nth 6 *board*))))

(defun reset-game ()
  "Funcion que reinicia el tablero a su estado original"
  (setq *board* '((1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)()(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)(1 5 10)())))

(defun game-ended? ()
  "Predicado el cual valida si el juego ya termino checando si alguna hilera esta completamente vacia"
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
  "Funcion que obtiene las canicas en una casilla"
  (nth casilla *board*))

;(defun same-slot? (casilla-actual)
;  "Predicado que valida que no se tire en la misma casilla"
;  (member casilla-actual *slots-shooted*))

(defun insert-ball (lista casilla)
  "Funcion que inserta las canicas de la forma (canica canica canica ...) en las casillas aledañas"
  (let ((casilla-siguiente (1+ casilla)))
    (loop for x in lista do
         (push x (nth casilla-siguiente *board*))
         (push casilla-siguiente *slots-shooted*)
         (setq casilla-siguiente (1+ casilla-siguiente))
         (if (> casilla-siguiente 13)
             (setq casilla-siguiente 0)
             ))))

(defun move-ball (casilla-actual)
  "Funcion que mueve las canicas de la casilla seleccionada"
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
  "Predicado que checa las casillas validas que el jugador humano puede escoger"
  (cond ((member casilla-actual '(6 7 8 9 10 11 12 13)) nil)
        ((null (get-balls casilla-actual)) nil)
        (T T)))

(defun human-turn()
  "Funcion en la cual se ejecuta el turno humano"
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

(defun valid-operator? (operador estado)
  "Predicado que valida si es el operador seleccionado es valido"
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
  "Funcion que aplica un operador de *ops* a un estado determinado"
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

(defun copy-board (tablero)
  "Funcion que crea una copia del tablero (por motivos de seguridad)"
  (let ((estado-copia nil))
  (loop for elemento in tablero do
    (setq estado-copia (cons elemento estado-copia)))
  (setq estado-copia  (reverse estado-copia))))

(defun move-machine-balls (tablero casilla-actual canicas-casilla)
  "Funcion que mueve las canicas de la IA"
  (let* ((canica-a-meter nil)
         (cont 0)
         (estado nil)
         (canicas nil)
         (longitud-canicas 0)
         (estado-copia nil)
         (best-canca 0)
         (shoot-again nil)
         (casilla-target (1+ casilla-actual)))

    (setq estado-copia (copy-board tablero))

    (loop for can in canicas-casilla do
         (setq canicas (cons can canicas)))

    (setq canicas(sort canicas #'>))
    (setq longitud-canicas (length canicas))
    (if ( >= (length canicas) (- 13 casilla-actual))
        (progn
          (setq best-canca (first canicas))
          (push best-canca (nth 13 estado-copia))
          ))

    ;Si la longitud de tus canicas es igual a la canica en la que te encuentras, la IA vuelve a tirar
     (if (= 0 (- (length canicas) (- 13 casilla-actual)))
         (setq shoot-again T)
         (setq shoot-again nil))

    (loop for canica in canicas do
         (setq canica-a-meter (pop (nth casilla-actual estado-copia)))
         (if (and ( = cont 0 ) ( = best-canca canica-a-meter))
             (progn
               (setq cont (1+ cont)))
             (progn
               (if (> casilla-target 12)
                   (setq casilla-target 0))
               (push canica-a-meter (nth casilla-target estado-copia))
               (setq casilla-target (1+ casilla-target))))
       finally (return (list estado-copia shoot-again casilla-actual)))))

(defun heuristic-function (estado)
  "Funcion de evaluación del estado la (BaseIA - BaseH)+(CanicasIA - CanicasH)"
  (let ((movimiento 0)
        (tablero (first estado)))
    (setq movimiento
          (+ (- (apply #'+ (nth 13 tablero)) (apply #'+ (nth 6 tablero)))
             (- (+ (apply #'+ (nth 7 tablero))(apply #'+ (nth 8 tablero))(apply #'+ (nth 9 tablero))
                   (apply #'+ (nth 10 tablero))(apply #'+ (nth 11 tablero))(apply #'+ (nth 12 tablero)))
                (+ (apply #'+ (nth 0 tablero))(apply #'+ (nth 1 tablero))(apply #'+ (nth 2 tablero))
                   (apply #'+ (nth 3 tablero))(apply #'+ (nth 4 tablero))(apply #'+ (nth 5 tablero))))))
    movimiento))

(defun change-player (jugador)
  "Funcion la cual cambia de jugador, si es 0 --> 1 (le toca a la IA) y si es 1 --> 0 (le toca al humano)"
  (case jugador
    (0 1)
    (1 0)))

(defun expand (estado)
  "Funcion que aplica todos los operadores de *ops* a un estado determinado y los regresa en una lista"
  (let* ((sucesores nil)
         (nuevo-estado nil)
         (estado-copia (copy-list estado)))
    (loop for operador in *ops* do
         (if (valid-operator? operador estado-copia)
             (progn
               (setq nuevo-estado (apply-operator operador estado-copia))
               (push nuevo-estado sucesores)))
       finally (return sucesores))))

(defun minimax (tablero profundidad max-profundidad jugador alpha beta)
  "Funcion Minimax con Poda Alfa y Beta"
  (if (= profundidad max-profundidad)
      (heuristic-function  tablero)
      (let ((sucesores (expand tablero)))
        (if (null sucesores)
            (heuristic-function tablero)
            (do ((nuevo-valor nil)
                 (mejor-mov (first sucesores)))
                ((null sucesores)
                 (if (= profundidad 0)
                     mejor-mov
                     beta))
              (setf nuevo-valor
                    (- (minimax
                        (first sucesores)
                        (1+ profundidad)
                        max-profundidad
                        (change-player jugador)
                        (- beta)
                        (- alpha))))
              (when (> nuevo-valor beta)
                (setf beta nuevo-valor)
                (setf mejor-mov (first sucesores)))
              (if (>= beta alpha)
                  (setf sucesores nil)
                  (setf sucesores (cdr sucesores))))))))

(defun machine-turn ()
  "Funcion en la cual se ejecuta el turno de la IA"
  (let ((movimiento nil)
        (movimiento-final nil)
        (shoot-again T))
    (loop until (null shoot-again) do
         (setq movimiento (minimax *board* 0 1 1 *alfa* (- *alfa*)))
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
  "Funcion main"
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

(play)
