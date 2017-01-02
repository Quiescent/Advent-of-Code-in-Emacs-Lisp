;;; day8 --- solving day8 -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'cl)

(defvar *q-screen* nil)
(defvar *q-x-dim*  nil)
(defvar *q-y-dim*  nil)

(defun q-make-screen (x y)
  "Produce a screen with the given X and Y dimensions."
  (let ((screen (make-vector y nil)))
    (dotimes (i y screen)
      (aset screen i (make-vector x nil)))))

(defun q-set-rect (x y)
  "Set all `pixels' in a rectange of dimensions X, Y on the top left."
  (dotimes (i x)
    (dotimes (l y)
      (aset (aref *q-screen* l) i t))))

(defun q-rotate-row (row dist)
  "Row-rotate the board on ROW by DIST pixels."
  (let* ((old-row (aref *q-screen* row))
         (new-row (seq-subseq old-row 0)))
    (dotimes (i *q-x-dim*)
      (aset new-row i (aref old-row (mod (+ i (- *q-x-dim* dist)) *q-x-dim*))))
    (aset *q-screen* row new-row)))

(defun q-rotate-col (col dist)
  "Col-rotate the board on COL by DIST pixels."
  (let* ((new-col (make-vector *q-y-dim* nil)))
    (dotimes (i *q-y-dim*)
      (aset new-col i (aref (aref *q-screen* (mod (+ i (- *q-y-dim* dist)) *q-y-dim*)) col)))
    (dotimes (i *q-y-dim*)
      (aset (aref *q-screen* i) col (aref new-col i)))))

(defun q-eval (inst)
  "Evaluate INST against the current board."
  (pcase inst
    (`(RECT ,x ,y)
     (q-set-rect x y))
    (`(ROTATE-ROW ,row ,dist)
     (q-rotate-row row dist))
    (`(ROTATE-COL ,col ,dist)
     (q-rotate-col col dist))))

(defun q-parse-instruction (inst)
  "Parse an instruction from INST."
  (cl-labels ((rvalue (x) (cadr (split-string x "="))))
    (pcase (split-string inst " " t)
      (`("rect" ,size)
       (let ((split-size (split-string size "x")))
         (list 'RECT (string-to-number (car split-size)) (string-to-number (cadr split-size)))))
      (`("rotate" "row"    ,row "by" ,distance)
       (list 'ROTATE-ROW (string-to-number (rvalue row)) (string-to-number distance)))
      (`("rotate" "column" ,col "by" ,distance)
       (list 'ROTATE-COL (string-to-number (rvalue col)) (string-to-number distance))))))

(defun q-count-pixels (screen)
  "Count the number of pixels in SCREEN."
  (apply #'+ (seq-map (lambda (row) (cl-count-if-not #'null row)) screen)))

(defun q-render-screen (screen)
  "Produce a human readable version of SCREEN."
  (let ((screen-rows ""))
    (seq-do (lambda (row)
              (progn (seq-do (lambda (col)
                               (setq screen-rows (concat screen-rows (if col "#" " "))))
                             row)
                     (setq screen-rows (concat screen-rows "\n"))))
            screen)
    screen-rows))

(defun q-answer-question (input)
  "Produce the count of pixels which are on after executing instructions in INPUT."
  (interactive "sInput: ")
  (let* ((instructions (mapcar #'q-parse-instruction (split-string input "\n" t)))
         (*q-x-dim*  50)
         (*q-y-dim*  6)
         (*q-screen* (q-make-screen *q-x-dim* *q-y-dim*)))
    (mapc #'q-eval instructions)
    (if (y-or-n-p "Display code? ")
        (message "%s" (q-render-screen *q-screen*))
      (message "%s" (q-count-pixels *q-screen*)))))

;; Simple answer:   121
;; Advanced answer: RURUCEOEIL

(provide 'day8)
;;; day8 ends here
