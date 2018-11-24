;;; day6-loop --- My solution to day6-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day6-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(require 'subr-x)

(defun day6-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with char-frequencies = `[,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)]
     for line in (split-string input-file "\n" t " ")
     do (cl-loop
           for char being the elements of line using (index i)
           do (puthash char
                       (1+ (gethash char (aref char-frequencies i) 0))
                       (aref char-frequencies i)))
     finally return (cl-loop
                       for table across char-frequencies
                       for char-keys = (hash-table-keys table)
                       for key-values = (mapcar (lambda (key) (cons (gethash key table)
                                                                    key))
                                                char-keys)
                       for sorted-key-values = (sort key-values
                                                     (lambda (x y)
                                                       (> (car x) (car y))))
                       concat (char-to-string (cdar sorted-key-values)))))

;; # PART 2:

(defun day6-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     with char-frequencies = `[,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)
                                     ,(make-hash-table :test #'eq)]
     for line in (split-string input-file "\n" t " ")
     do (cl-loop
           for char being the elements of line using (index i)
           do (puthash char
                       (1+ (gethash char (aref char-frequencies i) 0))
                       (aref char-frequencies i)))
     finally return (cl-loop
                       for table across char-frequencies
                       for char-keys = (hash-table-keys table)
                       for key-values = (mapcar (lambda (key) (cons (gethash key table)
                                                                    key))
                                                char-keys)
                       for sorted-key-values = (sort key-values
                                                     (lambda (x y)
                                                       (< (car x) (car y))))
                       concat (char-to-string (cdar sorted-key-values)))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day6-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day6-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day6-loop-part-1 input-1))
    (message "Part 2: %s\n" (day6-loop-part-2 input-2))))

(provide 'day6-loop)
;;; day6-loop ends here
