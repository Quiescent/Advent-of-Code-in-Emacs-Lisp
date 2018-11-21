;;; day3-loop --- My solution to day3-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day3-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day3-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     for triangle-line in (split-string input-file "\n" t " ")
     for (x y z) = (mapcar #'string-to-number (split-string triangle-line " " t " "))
     count (and (> (+ x y) z)
                (> (+ y z) x)
                (> (+ z x) y))))

;; # PART 2:

(defun day3-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (let* ((all-lines (cl-loop
                       for triangle-line in (split-string input-file "\n" t " ")
                       for line = (mapcar #'string-to-number (split-string triangle-line " " t " "))
                       collect line))
         (one-line (append (mapcar #'car   all-lines)
                           (mapcar #'cadr  all-lines)
                           (mapcar #'caddr all-lines))))
    (cl-loop
       for sublist on one-line by #'cl-cdddr
       for (x y z) = `(,(car sublist) ,(cadr sublist) ,(caddr sublist))
       count (and (> (+ x y) z)
                  (> (+ y z) x)
                  (> (+ z x) y)))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day3-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day3-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day3-loop-part-1 input-1))
    (message "Part 2: %s\n" (day3-loop-part-2 input-2))))

(provide 'day3-loop)
;;; day3-loop ends here
