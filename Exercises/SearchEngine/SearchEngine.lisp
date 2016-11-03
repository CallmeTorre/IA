;------------------------------------------
;| Jesús Alexis Torreblanca Faces         |
;| Inteligencia Artificial                |
;| Motor de consulta                      |
;------------------------------------------

(defparameter *knowledge-vector* (make-array 100
                                            :adjustable T
                                            :element-type 'list))

(defparameter *information* '((jugador (0 41))
                              (estadio (42 48))
                              (entrenador (50 55))
                              (mascota (56 60))
                              (relacion (61 121))))

(defparameter *answer* nil)
(defparameter *final-answer* nil)

(defun reset-all ()
  (setq *answer* nil
        *final-answer* nil))

(defun read-knowledge (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
          (adjust-array *knowledge-vector* numrows)
          (read-line stream nil nil)
          (dotimes (row numrows *knowledge-vector*)
            (setf (aref *knowledge-vector* row) (read stream nil nil))))))

(defun get-class (clase informacion)
  (cond ((null informacion) nil)
        ((equal (first (first informacion)) clase) (second (first informacion)))
        (T (get-class clase (rest informacion)))))

(defun get-info-existencial (inicio fin attr base-conocimiento)
  (loop for x from inicio to fin do
       (if (null attr)
           (push (aref base-conocimiento x) *final-answer*)
           (progn
             (loop for i from 1 to (1- (length (aref base-conocimiento x))) do
                  (let ((clave (first (nth i (aref base-conocimiento x))))
                        (valor (rest (nth i (aref base-conocimiento x)))))
                    (loop for atributo in attr do
                         (if (and (equal (first atributo) clave)(equal (rest atributo) valor))
                             (push 1 *answer*)))
                    (if (= (apply #'+ *answer*)(length attr))
                        (push (aref base-conocimiento x) *final-answer*))
                    (if (= (length attr) 1)
                        (setq *answer* '(0)))))
             (setq *answer* '(0))))))

(defun get-info-universal (inicio fin attr base-conocimiento)
  (loop for x from inicio to fin do
       (if (null attr)
           (push (aref base-conocimiento x) *final-answer*)
           (progn
             (loop for i from 1 to (1- (length (aref base-conocimiento x))) do
                  (let ((clave (first (nth i (aref base-conocimiento x))))
                        (valor (rest (nth i (aref base-conocimiento x)))))
                    (loop for atributo in attr do
                         (if (and (equal (first atributo) clave)(not (equal (rest atributo) valor)))
                             (push 1 *answer*)))
                    (if (= (apply #'+ *answer*)(length attr))
                        (push (aref base-conocimiento x) *final-answer*))
                    (if (= (length attr) 1)
                        (setq *answer* '(0)))))
             (setq *answer* '(0))))))

(defun search-engine (consulta)
  (let* ((operador (first consulta))
         (busqueda (rest consulta))
         (clase (first busqueda))
         (valor-clase (rest clase))
         (posiciones-clase (get-class valor-clase *information*))
         (atributos (rest busqueda)))
    (cond ((equal operador '+)
           (progn
             (if (null atributos)
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) nil *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*))))
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) atributos *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*)))))))
          ((equal operador '-)
           (progn
             (if (null atributos)
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) nil *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*))))
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) atributos *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*)))))))
          ((equal operador '*)
           (progn
             (if (null atributos)
                 (progn
                   (get-info-universal (first posiciones-clase) (second posiciones-clase) nil *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*))))
                 (progn
                   (get-info-universal (first posiciones-clase) (second posiciones-clase) atributos *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*)))))))
          ((equal operador '/)
           (progn
             (if (null atributos)
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) nil *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*))))
                 (progn
                   (get-info-existencial (first posiciones-clase) (second posiciones-clase) atributos *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*)))))))
          (T "Operador Erroneo"))))

(defun main ()
  (let ((consulta nil))
    (read-knowledge "BaseDeConocimiento.txt")
    (loop
       (reset-all)
       (print "Escriba la consulta")
       (setq consulta (read))
       (if (equal consulta 'exit)
           (Return))
       (search-engine consulta))))

(main)
