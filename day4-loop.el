;;; day4-loop --- My solution to day4-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day4-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(require 'subr-x)

(defun day4-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     for sums = (make-hash-table :test #'eq)
     for (sectors rest) = (split-string line "\\[" t " ")
     for checksum = (cl-remove ?\] rest)
     for letters = (cl-remove-if (lambda (x) (member x '(?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))) sectors)
     for id = (string-to-number
               (cl-remove-if-not (lambda (x) (member x '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))) sectors))
     do (cl-loop
           for letter across letters
           for current-count = (gethash letter sums 0)
           do (puthash letter (1+ current-count) sums))
     for key-values = (cl-mapcar #'cons (cl-mapcar (lambda (key) (gethash key sums))
                                                   (hash-table-keys sums))
                                        (hash-table-keys sums))
     for sorted-key-values = (cl-sort key-values (lambda (x y)
                                                   (or (> (car x)
                                                          (car y))
                                                       (and (= (car x)
                                                               (car y))
                                                            (< (cdr x)
                                                               (cdr y))))))
     for most-common-characters = (apply #'string
                                           (cl-sort
                                            (cl-subseq (mapcar #'cdr sorted-key-values)
                                                       0
                                                       (length checksum))
                                              #'<))
     sum (if (string-equal most-common-characters (cl-sort checksum #'<))
             id
             0)))

;; # PART 2:

(defun day4-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     for line in (split-string input-file "\n" t " ")
     for sums = (make-hash-table :test #'eq)
     for (sectors rest) = (split-string line "\\[" t " ")
     for id = (string-to-number
               (cl-remove-if-not (lambda (x) (member x '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))) sectors))
     for decrypted = (mapcar (lambda (x) (if (eq x ?\ ) ?\  (+ 97 (mod (+ (- x 97) id) 26)))) sectors)
     do (princ (format "%s %s\n" (apply #'string decrypted) id))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day4-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day4-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day4-loop-part-1 input-1))
    (message "Part 2: %s\n" (day4-loop-part-2 input-2))))

(provide 'day4-loop)
;;; day4-loop ends here
