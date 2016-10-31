(defparameter *knowledge-vector* (make-array 100
                                            :adjustable T
                                            :element-type 'list))

(defun read-knowledge (filename)
  (with-open-file (stream filename)
    (let ((numrows (read stream)))
          (adjust-array *knowledge-vector* numrows)
          (read-line stream nil nil)
          (dotimes (row numrows *knowledge-vector*)
            (setf (aref *knowledge-vector* row) (read stream nil nil))))))

  (read-knowledge "/Users/Callmetorre/Documents/AI/Exercises/SearchEngine/BaseDeConocimiento.txt")

(print *knowledge-vector*)
