(defparameter *open* '())
(defparameter *memory* '())
(defparameter *id* 0)
(defparameter *ops* '((:arriba "^")
		     (:abajo "v")
		     (:izquierda "<")
		     (:derecha ">")))
(defparameter *current-ancestor*  nil)
(defparameter *solucion*  nil)
(defparameter *contadorNodos* 0)
(defparameter *contadorExpandir* 0)
(defparameter *contadorFronteraBusqueda* 0)
(defparameter *tiempoInicial* 0)
(defparameter *tiempoFinal* 0)
(defparameter *meta* nil)

(defun where-is-x (estado)
  "Funcion que nos permite saber donde esta x"
  (let ((filax 0)
	(casillax 0))
    (dolist (fila estado)
      (if (or (equal (first fila) 'x) (equal (second fila) 'x) (equal (third fila) 'x))
	  (cond ((equal (first fila) 'x) (return (list filax casillax)))
		((equal (second fila) 'x) (return (list filax (setq casillax (1+ casillax)))))
		(T (return (list filax (setq casillax (+ 2 casillax))))))
	  (setq filax (1+ filax))))))


(defun up (estado)
  "Funcion que mueve x hacia arriba en el tablero"
  (let* ((filax (first (where-is-x estado)))
	 (casillax (second (where-is-x estado)))
	 (filatarget (nth (1- filax) estado))
	 (elemtarget (nth casillax filatarget))
	 (filaconx (nth filax estado)))
    (if (= filax 1)
	(list (substitute 'x elemtarget filatarget)
	      (substitute elemtarget 'x filaconx)
	      (third estado))
	(list (first estado)
	      (substitute 'x elemtarget filatarget)
	      (substitute elemtarget 'x filaconx)))))

(defun down (estado)
  "Funcion que mueve x hacia abajo en el tablero"
   (let* ((filax (first (where-is-x estado)))
	 (casillax (second (where-is-x estado)))
	 (filatarget (nth (1+ filax) estado))
	 (elemtarget (nth casillax filatarget))
	 (filaconx (nth filax estado)))
     (if (= filax 0)
	 (list (substitute elemtarget 'x filaconx)
	       (substitute 'x elemtarget filatarget)
	       (third estado))
	(list (first estado)
	      (substitute elemtarget 'x filaconx)
	      (substitute 'x elemtarget filatarget)))))

(defun left (estado)
  "Funcion que mueve x hacia la izquierda en el tablero"
  (let* ((filax (first (where-is-x estado)))
	 (casillax (second (where-is-x estado)))
	 (filaconx (nth filax estado))
	 (elemtarget (nth (1- casillax) filaconx)))
    (cond ((= filax 0)
	   (list (substitute 'x elemtarget (substitute elemtarget 'x filaconx) :count 1)
		 (second estado)
		 (third estado)))
	  ((= filax 1)
	   (list (first estado)
		 (substitute 'x elemtarget (substitute elemtarget 'x filaconx) :count 1)
		 (third estado)))
	  (T (list (first estado)
		   (second estado)
		   (substitute 'x elemtarget (substitute elemtarget 'x filaconx) :count 1))))))

(defun right (estado)
  "Funcion que mueve x hacia la derecha en el tablero"
  (let* ((filax (first (where-is-x estado)))
	 (casillax (second (where-is-x estado)))
	 (filaconx (nth filax estado))
	 (elemtarget (nth (1+ casillax) filaconx)))
    (cond ((= filax 0)
	   (list (substitute elemtarget 'x (substitute 'x elemtarget filaconx) :count 1)
		 (second estado)
		 (third estado)))
	  ((= filax 1)
	   (list (first estado)
		 (substitute elemtarget 'x (substitute 'x elemtarget filaconx) :count 1)
		 (third estado)))
	  (T (list (first estado)
		   (second estado)
		   (substitute elemtarget 'x (substitute 'x elemtarget filaconx) :count 1))))))

(defun valid-operator? (operador estado)
  "Predicado que regresa si el operador es valido o no"
  (let ((filax (first (where-is-x estado)))
	(casillax (second (where-is-x estado)))
	(ops (first operador)))
    (cond ((equal ops :arriba) (if (= filax 0) nil T))
	  ((equal ops :abajo) (if (= filax 2) nil T))
	  ((equal ops :izquierda) (if (= casillax 0) nil T))
	  ((equal ops :derecha) (if (= casillax 2) nil T))
	  (T "Error"))))

(defun apply-operator (operador estado)
  "Funcion que aplica el operador especificado al estado"
  (let ((ops (first operador)))
    (case ops
      (:arriba (up estado))
      (:abajo (down estado))
      (:izquierda (left estado))
      (:derecha (right estado))
      (T "Error"))))

(defun smooth (lista)
  "Funcion auxiliar que nos permite 'aplanar' una lista"
  (cond ((null lista) nil)
	((atom (first lista)) (cons (first lista) (smooth (rest lista))))
	(T (append (smooth (first lista)) (smooth (rest lista))))))

(defun aux-count-wrong-pieces (estado meta cont)
  "Funcion auxiliar que nos permite contar el numero de piezas desacomodadas en el estado actual"
  (cond ((null estado) cont)
	((equal (first estado) 'x) (aux-count-wrong-pieces (rest estado) (rest meta) cont))
	((equal (first estado) (first meta)) (aux-count-wrong-pieces (rest estado) (rest meta) cont))
	(T (+ (aux-count-wrong-pieces (rest estado) (rest meta) (1+ cont))))))

(defun count-wrong-pieces (estado meta)
  "Funcion la cual nos devuelve el numero de piezas desacomodadas"
  (aux-count-wrong-pieces (smooth estado) (smooth meta) 0))

(defun expand (estado)
  "Funcion que expande el estado"
  (incf *contadorExpandir*)
  (let ((descendientes nil)
	(nuevo-estado nil))
    (dolist (op *ops* descendientes)
      (if (valid-operator? op estado)
			   (progn
			     (setq nuevo-estado (apply-operator op estado))
			     (setq descendientes (cons (list nuevo-estado op) descendientes)))))))

(defun create-node (estado operador fichas)
  "Funcion que crea los nodos del arbol"
  (incf *id*)
  (incf *contadorNodos*)
  (list (1- *id*) estado *current-ancestor* (first operador) fichas))

(defun order-open ()
  "Funcion que permite reordenar la frontera de busqueda"
  (setq *open* (stable-sort *open* #'< :key #'(lambda (x) (fifth x)))))

(defun insert-to-open (estado operador metodo)
  "Funcion que inserta el nodo en la frontera de busqueda dependiendo del metodo que se le haya indicado"
  (let ((nodo '()))
    (cond ((equal metodo :wrong-pieces)
	   (setq nodo (create-node estado operador (count-wrong-pieces estado *meta*)))
	   (push nodo *open*)
	   (order-open))
	  ((equal metodo :moves-left)
	   (setq nodo (create-node estado operador (random 5))) ;;(count-moves estado *meta*)
	   (push nodo *open*)
	   (order-open))
	  ((equal metodo :random-value)
	   (setq nodo (create-node estado operador (random 5)))
	   (push nodo *open*)
	   (order-open))
	  ((equal metodo :custom-value)
	   (setq nodo (create-node estado operador (random 3)))
	   (push nodo *open*)
	   (order-open))
	  (T Nil))))

(defun get-from-open ()
  "Funcion que permite obtener un elemento de la frontera de busqueda"
  (pop *open*))

(defun insert-to-memory (nodo)
  "Funcion que permite insertar un elemento a la memoria"
  (push nodo *memory*))

(defun remember-state-memory? (estado memoria)
  "Predicado que permite saber si un estado esta en la memoria"
  (cond ((null  memoria)  Nil)
	((equal  estado  (second (first memoria)))  T)
	(T  (remember-state-memory?  estado  (rest memoria)))))

(defun remember-state-open? (estado open)
  "Predicado que permite saber si un estado esta en la frontera de busqueda"
  (cond ((null open) Nil)
	((equal estado (second (first open))) T)
	(T (remember-state-open? estado (rest open)))))

(defun filter-memories (lista-estados-y-ops)
  "Funcion que permite filtrar los estados que aun no estan en la memoria"
  (cond ((null  lista-estados-y-ops)  Nil)
	((remember-state-memory? (first (first  lista-estados-y-ops)) *memory*)
	 (filter-memories  (rest  lista-estados-y-ops)))
	(T  (cons (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))))

(defun filter-open (lista-estados-y-ops)
  "Funcion que permite filtrar los estados que aun no estan en la memoria"
  (cond ((null  lista-estados-y-ops)  Nil)
	((remember-state-open? (first (first  lista-estados-y-ops)) *open*)
	 (filter-open  (rest  lista-estados-y-ops)))
	(T  (cons (first lista-estados-y-ops) (filter-open  (rest  lista-estados-y-ops))))))

(defun extract-solution (nodo)
  "Funcion que extrae la solucion"
  (labels ((locate-node (id lista)
	     (cond ((null lista) Nil)
		   ((equal id (first (first lista))) (first lista))
		   (T (locate-node id (rest lista))))))
    (let ((current (locate-node (first nodo) *memory*)))
      (loop while (not (null current)) do
	   (push current *solucion*)
	   (setq current (locate-node (third current) *memory*))))
    *solucion*))

(defun display-solution (lista-nodos)
  "Funcion la cual despliega la solucion"
  (setq *tiempoFinal* (get-internal-real-time))
  (format  t "Nodos creados ~A ~%" *contadorNodos*)
  (format  t "Nodos expandidos ~A ~%" *contadorExpandir*)
  (format  t "Longitud maxima de frontera de busqueda ~A ~%" *contadorFronteraBusqueda*)
  (format  t "Tiempo para encontrar la solucion: ~A~%" (/ (- *tiempoFinal* *tiempoInicial*) internal-time-units-per-second))
  (format  t  "Solucion con ~A  pasos~%" (1- (length  lista-nodos)))
  (let ((nodo nil))
    (dotimes (i (length lista-nodos))
      (setq nodo (nth i lista-nodos))
      (if (= i 0)
	  (format t "Inicio en: ~A~%" (second  nodo))
	  (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (fourth  nodo)  (second  nodo))))))

(defun reset-all ()
  "Funcion de limpiado"
  (setq  *open*  nil)
  (setq  *memory*  nil)
  (setq  *id*  0)
  (setq *contadorNodos* 0)
  (setq *contadorExpandir* 0)
  (setq *contadorFronteraBusqueda* 0)
  (setq *tiempoInicial* 0)
  (setq *tiempoFinal* 0)
  (setq  *current-ancestor*  nil)
  (setq  *solucion*  nil))

(defun informed-search (inicio meta metodo)
  "Funcion main"
  (reset-all)
  (setq *tiempoInicial* (get-internal-real-time))
  (let ((nodo nil)
	(estado nil)
        (sucesores '())
        (meta-encontrada nil))
    (setq *meta* meta)
    (insert-to-open inicio nil metodo)
    (loop until (or meta-encontrada (null *open*))
       do (setq nodo (get-from-open)
		estado (second nodo))
	 ;get-max-in-open
		(insert-to-memory nodo)
		(cond ((equal meta estado)
		       (display-solution (extract-solution nodo))
		       (setq meta-encontrada T))
		      (T (setq *current-ancestor* (first nodo))
			 (setq sucesores (expand estado))
			 (setq sucesores (filter-open sucesores))
			 (setq sucesores (filter-memories sucesores))
			 (loop for elem in sucesores
			    do (insert-to-open (first elem) (second elem) metodo)))))))


(informed-search '((2 8 3)(1 4 5)(7 x 6)) '((1 2 3)(8 x 4)(7 6 5)) :wrong-pieces )
