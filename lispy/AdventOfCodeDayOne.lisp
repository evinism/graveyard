(defparameter fname "data/AdventOfCode/d1.txt")

(defun read-into-lines (fname)
    (let ((in (open fname)))
    (loop for line = (read-line in nil)
        while line collect line)))

(defun to-signed-ints (lines)
    (mapcar #'parse-integer lines))

(defun solve-day-one-part-one ()
    (format t "~A~%" (apply #'+ 
        (to-signed-ints (read-into-lines fname)))))

(defun to-freqs (input-list &optional (init-value 0))
    (reverse 
        (reduce #'(lambda (acc cur) 
            (cons 
                (+ 
                    (first acc)
                    cur)
                acc))
            input-list
            :initial-value (list init-value))))

(defun get-freq-sequence (fname)
    (to-freqs (to-signed-ints (read-into-lines fname))))

;; garbage lisp because i dont know how to write it
(defun find-first-dupe-aux (ht input-list prev)
    (let ((head (first input-list)))
        (if head
            (if (gethash head ht)
                head
                (progn
                    (setf (gethash head ht) t)
                    (find-first-dupe-aux ht (rest input-list) head)))
            (find-first-dupe-aux 
                ht 
                (rest (to-freqs (to-signed-ints (read-into-lines fname)) prev))
                prev))))

;; really garbage implementation because idk the better data structures yet
(defun find-first-dupe (input-list)
    (find-first-dupe-aux (make-hash-table) input-list 0))

(defun solve-day-one-part-two ()
    (find-first-dupe (get-freq-sequence fname)))
