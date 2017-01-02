;;; day18 --- Solving day 18 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; ^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^.

;;; Code:

(defvar *q-total-rows* nil)

(defun q-parse-row (row)
  "Parse ROW into a vector with SAFE and TRAP tiles."
  (apply #'vector (seq-map (lambda (x)
                             (pcase x
                               ("." 'SAFE)
                               ("^" 'TRAP)))
                           (split-string row "" t))))

(defun q-repeat (xs times)
  "Replicate sequence XS, TIMES."
  (let (result)
    (dotimes (_ times result)
      (push (seq-subseq xs 0) result))))

(defun q-parse (input)
  "Parse INPUT into a map, where the first row is filled in."
  (apply #'vector (nconc (list (q-parse-row (car (split-string input "\n" t))))
                         (q-repeat (make-vector (length input) nil) (1- *q-total-rows*)))))

(defun q-expand-row (row board)
  "Fill in ROW from BOARD using the values in the previous row."
  (let* ((curr-row (aref board row))
         (prev-row (aref board (1- row)))
         (length   (length curr-row)))
    (dotimes (i length board)
      (let  ((left   (if (= 0 i) 'SAFE (aref prev-row (1- i))))
             (centre (aref prev-row i))
             (right  (if (= (1- length) i) 'SAFE (aref prev-row (1+ i)))))
        (aset curr-row i
              (pcase `(,left ,centre ,right)
                (`(TRAP TRAP SAFE) 'TRAP)
                (`(SAFE TRAP TRAP) 'TRAP)
                (`(TRAP SAFE SAFE) 'TRAP)
                (`(SAFE SAFE TRAP) 'TRAP)
                (_                 'SAFE)))))))

(defun q-expand-board (board)
  "Starting from row 1, expand BOARD by filling in traps and safe blocks."
  (dotimes (i (1- *q-total-rows*) board)
    (q-expand-row (1+ i) board)))

(defun q-count-safe-blocks (board)
  "Count the number of safe blocks in BOARD."
  (seq-reduce #'+
              (seq-map (lambda (row)
                         (cl-count-if (lambda (x) (eq 'SAFE x)) row))
                       board)
              0))

(defun q-answer-question (rows input)
  "Count safe blocks on the floor of ROWS tiles given the first row, INPUT."
  (interactive "nRows: \nsInput: ")
  (let* ((*q-total-rows* rows)
         (board          (q-parse input)))
    (q-expand-board board)
    (message "%s" (q-count-safe-blocks board))))

;; Safe block count: 1926
;; With 40K rows:    19986699

(provide 'day18)
;;; day18 ends here
