
;Cargamos la libreria para comenzar a trabajar con ella
(load "maze_lib.lisp")

;Algoritmo al menu de la pagina principal
(add-algorithm 'depth-first)
(add-algorithm 'breath-first)
(add-algorithm 'best-first)
(add-algorithm 'A*)

;Permite saber para cada problema la frontera de busqueda y memoria
(defparameter *open*  '())
(defparameter *memory* '())

;Permite almacenar los datos del laberinto
(defparameter *id* 0)
(defparameter *current-ancestor* nil)
(defparameter *solution* nil)
(defparameter *filas*  nil)
(defparameter *columnas* nil)
(defparameter *sol* nil)

;Permite saber de donde vino el puente
(defparameter *puente* 0)
;Definicion de operadores
(defparameter *operadores* '((:Mover-Arriba 0)
                       (:Mover-Arriba-Derecha 1)
                       (:Mover-Derecha 2)
                       (:Mover-Abajo-Derecha 3)
                       (:Mover-Abajo 4)
                       (:Mover-Abajo-Izquierda 5)
                       (:Mover-Izquierda 6)
                       (:Mover-Arriba-Izquierda 7)))

;[Funcion] Permite resetear todo
(defun reset-all ()
  (setq *open*   nil)
  (setq *memory*  nil)
  (setq *id*  0)
  (setq *sol* nil)
  (setq *puente* 0)
  (setq *current-ancestor*  nil)
  (setq *solution*  nil))

;[Funcion] Permite crear los nodos necesarios
(defun create-node (estado operador importancia)
  (incf *id*)
  (list (1- *id*) importancia estado *current-ancestor* (second operador)))

(defun get-distance (estado)
    (+ (- (max (aref estado 0) (aref *goal* 0))
          (min (aref estado 0) (aref *goal* 0)))
        (- (max (aref estado 1) (aref *goal* 1))
           (min (aref estado 1) (aref *goal* 1)))))

(defun insert-to-open (estado operador metodoBusqueda)
  (let* ((nodo '()))
    (cond ((eql metodoBusqueda :depth-first)
          (setq))))
