;;; day8-loop --- My solution to day8-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day8-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)
(require 'seq)

(defun make-matrix (x y)
  "Create a matrix of dimension [X, Y]."
  (let ((result (make-vector y nil)))
    (dotimes (i y result)
      (aset result i (make-vector x nil)))))

(defun matrix-ref (matrix x y)
  "Produce MATRIX at [X, Y]."
  (aref (aref matrix y) x))

(defun matrix-set (matrix x y val)
  "Set the value in MATRIX at [X, Y] to VAL."
  (aset (aref matrix y) x val))

(defun transpose-matrix (matrix)
  "Transpose MATRIX."
  (let* ((x          (length (aref matrix 0)))
         (y          (length matrix))
         (new-matrix (make-matrix y x)))
    (cl-loop for i below x
       do (cl-loop for j below y
             do (matrix-set new-matrix j i (matrix-ref matrix i j)))
       finally return new-matrix)))

(defun rotate (array amount)
  "Rotate ARRAY forwards by AMOUNT."
  (let ((amt (mod (- 0 amount) (length array))))
    (seq-concatenate 'vector
                     (seq-subseq array amt)
                     (seq-subseq array 0 amt))))

(defun render (input-file)
  "Given INPUT-FILE, produce the display which it generates."
  (cl-loop
     with display = [".................................................."
         ".................................................."
         ".................................................."
         ".................................................."
         ".................................................."
         ".................................................."]
     for line in (split-string input-file "\n" t " ")
     do (pcase (split-string line " " t " ")
          (`("rect" ,dim)
            (pcase (split-string dim "x" t " ")
              (`(,x-str ,y-str)
                (let ((x (string-to-number x-str))
                      (y (string-to-number y-str)))
                  (cl-loop for i below x
                     do (cl-loop for j below y
                           do (matrix-set display i j ?#)))))))
          (`("rotate" "row" ,row-str "by" ,amount-str)
            (let ((row (string-to-number (substring row-str 2)))
                  (amt (string-to-number amount-str)))
              (aset display row (rotate (aref display row) amt))))
          (`("rotate" "column" ,col-str "by" ,amount-str)
            (let ((col (string-to-number (substring col-str 2)))
                  (amt (string-to-number amount-str)))
              (setq display
                    (let ((transposed (transpose-matrix display)))
                      (aset transposed
                            col
                            (rotate (aref transposed col) amt))
                      (transpose-matrix transposed))))))
     finally return (cl-loop for line across display
                       concat (concat (apply #'string (cl-loop for char across line collect char)) "\n"))))

(defun day8-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-count ?# (render input-file)))

;; # PART 2:

(defun day8-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (render input-file))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day8-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day8-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1:%s" (day8-loop-part-1 input-1))
    (message "Part 2:\n%s\n" (day8-loop-part-2 input-2))))

(provide 'day8-loop)
;;; day8-loop ends here

