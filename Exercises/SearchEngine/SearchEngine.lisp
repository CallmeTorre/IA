;------------------------------------------
;| Jesús Alexis Torreblanca Faces         |
;| Inteligencia Artificial                |
;| Motor de consulta                      |
;------------------------------------------

(defparameter *knowledge-vector* (make-array 100
                                            :adjustable T
                                            :element-type 'list))

(defparameter *answer* nil)
(defparameter *final-answer* nil)
(defparameter *opertor* nil)
(defparameter *value* nil)

(defun reset-all ()
"Funcion que reinicia las variables globale spara las nuevas consultas"
  (setq *answer* nil
        *final-answer* nil))

(defun read-knowledge (filename)
"Funcion que permite obtener la información de la base de conocimientos"
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
          (adjust-array *knowledge-vector* numrows)
          (read-line stream nil nil)
          (dotimes (row numrows *knowledge-vector*)
            (setf (aref *knowledge-vector* row) (read stream nil nil))))))

(defun notEqual (valor1 valor2)
"Funcion que imita el comportamiento de hacer un Not Equal"
  (not (equal valor1 valor2)))

(defun conditional (expresion)
"Funcion que permite saber si se esta usando reducción de terminos con los operadores lógicos básicos"
  (let* ((exp (write-to-string expresion))
         (len-exp (length exp))
         (exp-igual nil)
         (condicion-extra nil)
         (exp-sin-igual nil))

    (if (equal (subseq exp 0 1) "[")
        (setq exp-igual (subseq exp 0 3)
              exp-sin-igual (subseq exp 0 2)))

    (cond ((string-equal exp-igual '[>=)
           (progn
             (setq *opertor* #'>=
                   *value* (parse-integer (subseq exp 3 (1- len-exp))))))
          ((string-equal exp-igual '[!=)
           (setq *opertor*  #'notEqual)
           (setq condicion-extra (read-from-string (subseq exp 3 (1- len-exp))))
           (if (numberp condicion-extra)
               (progn
                 (setq *opertor* #'/=)
                 (setq *value* (parse-integer (subseq exp 3 (1- len-exp)))))
               (setq *value* (intern (subseq exp 3 (1- len-exp))))))
          ((string-equal exp-igual '[<=)
           (progn
             (setq *opertor* #'<=
                   *value* (parse-integer (subseq exp 3 (1- len-exp))))))
          ((string-equal exp-sin-igual '[>)
           (progn
             (setq *opertor* #'>
                   *value* (parse-integer (subseq exp 2 (1- len-exp))))))
          ((string-equal exp-sin-igual '[<)
           (progn
             (setq *opertor* #'<
                   *value* (parse-integer (subseq exp 2 (1- len-exp))))))
          (T (setq *opertor* nil
                   *value* nil)))))

(defun get-info-existencial (attr base-conocimiento)
"Funcion que realiza las consultas para el cuantificador existencial"
  (loop for x from 0 to 128 do
             (loop for i from 0 to (1- (length (aref base-conocimiento x))) do
                  (let ((clave (first (nth i (aref base-conocimiento x))))
                        (valor (rest (nth i (aref base-conocimiento x)))))
                    (loop for atributo in attr do
                         (conditional (rest atributo))
                         (if (equal (first atributo) clave)
                             (if (null *opertor*)
                                 (if (equal (rest atributo) valor)
                                     (push 1 *answer*))
                                 (progn
                                   (if (funcall *opertor* valor *value*)
                                       (push 1 *answer*))))))))
             (if (= (apply #'+ *answer*)(length attr))
                 (push (aref base-conocimiento x) *final-answer*))
             (setq *answer* nil
                   *opertor* nil
                   *value* nil)))

(defun get-info-universal (attr base-conocimiento)
"Funcion que realiza las consultas para el cuantificador universal"
  (loop for x from 0 to 128 do
             (loop for i from 0 to (1- (length (aref base-conocimiento x))) do
                  (let ((clave (first (nth i (aref base-conocimiento x))))
                        (valor (rest (nth i (aref base-conocimiento x)))))
                    (loop for atributo in attr do
                         (conditional (rest atributo))
                         (if (equal (first atributo) clave)
                             (if (null *opertor*)
                                 (if (not (equal (rest atributo) valor))
                                     (setq *answer* t))
                                 (progn
                                   (if (not (funcall *opertor* valor *value*))
                                       (setq *answer* t)
                                       (setq *answer* nil))))))))
             (if (not (null *answer*))
                 (push (aref base-conocimiento x) *final-answer*))
             (setq *answer* nil
                   *opertor* nil
                   *value* nil)))

(defun search-engine (consulta)
"Funcion que recibe la consulta y determina que tipo de consulta esta realizando el usuario"
  (let* ((operador (first consulta))
         (busqueda (rest consulta)))
          (cond ((equal operador '+)
                   (get-info-existencial busqueda *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*))))
          ((equal operador '-)
                   (get-info-existencial busqueda *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*))))
          ((equal operador '*)
                   (get-info-universal busqueda  *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "True")
                       (progn
                         (print "False")
                         (print *final-answer*))))
          ((equal operador '/)
                   (get-info-universal busqueda *knowledge-vector*)
                   (if (null *final-answer*)
                       (print "False")
                       (progn
                         (print "True")
                         (print *final-answer*))))
          (T "Operador Erroneo"))))

(defun main ()
"Función Principal"
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
