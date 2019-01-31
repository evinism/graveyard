(defparameter fname "data/AdventOfCode/d1.txt")

(defun read-into-lines (fname)
    (let ((in (open fname)))
    (loop for line = (read-line in nil)
        while line collect line)))


(defun hash-keys (hash-table)
    (loop for key being the hash-keys of hash-table collect key))

(defun str-to-list (str)
    (let ((strlen (length str)))
        (loop for x from 1 to strlen
              for y = (char str (- x 1))
              collect y)))

(defun update-count (ht char)
    (let ((existing (gethash char ht)))
        (if existing
            (setf 
                (gethash char ht) (+ existing 1))
            (setf 
                (gethash char ht) 1))
        ht))

(defun to-letter-count (str)
    (let ((ht (make-hash-table))
          (strlist (str-to-list str)))
        (reduce #'update-count strlist :initial-value ht)))

(defun )
