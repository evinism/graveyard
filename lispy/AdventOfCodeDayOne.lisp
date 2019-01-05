(defparameter fname "data/AdventOfCode/d1.txt")

(defun read-into-lines (fname)
    (let ((in (open fname)))
    (loop for line = (read-line in nil)
        while line collect line)))

(defun to-signed-ints (lines)
    (mapcar #'parse-integer lines))

(format t "~A~%" (apply #'+ 
    (to-signed-ints (read-into-lines fname))))
