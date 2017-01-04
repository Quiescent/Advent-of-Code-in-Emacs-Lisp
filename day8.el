;;; day8 --- solving day8 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input
;; rect 1x1
;; rotate row y=0 by 10
;; rect 1x1
;; rotate row y=0 by 10
;; rect 1x1
;; rotate row y=0 by 5
;; rect 1x1
;; rotate row y=0 by 3
;; rect 2x1
;; rotate row y=0 by 4
;; rect 1x1
;; rotate row y=0 by 3
;; rect 1x1
;; rotate row y=0 by 2
;; rect 1x1
;; rotate row y=0 by 3
;; rect 2x1
;; rotate row y=0 by 2
;; rect 1x1
;; rotate row y=0 by 3
;; rect 2x1
;; rotate row y=0 by 5
;; rotate column x=0 by 1
;; rect 4x1
;; rotate row y=1 by 12
;; rotate row y=0 by 10
;; rotate column x=0 by 1
;; rect 9x1
;; rotate column x=7 by 1
;; rotate row y=1 by 3
;; rotate row y=0 by 2
;; rect 1x2
;; rotate row y=1 by 3
;; rotate row y=0 by 1
;; rect 1x3
;; rotate column x=35 by 1
;; rotate column x=5 by 2
;; rotate row y=2 by 5
;; rotate row y=1 by 5
;; rotate row y=0 by 2
;; rect 1x3
;; rotate row y=2 by 8
;; rotate row y=1 by 10
;; rotate row y=0 by 5
;; rotate column x=5 by 1
;; rotate column x=0 by 1
;; rect 6x1
;; rotate row y=2 by 7
;; rotate row y=0 by 5
;; rotate column x=0 by 1
;; rect 4x1
;; rotate column x=40 by 2
;; rotate row y=2 by 10
;; rotate row y=0 by 12
;; rotate column x=5 by 1
;; rotate column x=0 by 1
;; rect 9x1
;; rotate column x=43 by 1
;; rotate column x=40 by 2
;; rotate column x=38 by 1
;; rotate column x=15 by 1
;; rotate row y=3 by 35
;; rotate row y=2 by 35
;; rotate row y=1 by 32
;; rotate row y=0 by 40
;; rotate column x=32 by 1
;; rotate column x=29 by 1
;; rotate column x=27 by 1
;; rotate column x=25 by 1
;; rotate column x=23 by 2
;; rotate column x=22 by 1
;; rotate column x=21 by 3
;; rotate column x=20 by 1
;; rotate column x=18 by 3
;; rotate column x=17 by 1
;; rotate column x=15 by 1
;; rotate column x=14 by 1
;; rotate column x=12 by 1
;; rotate column x=11 by 3
;; rotate column x=10 by 1
;; rotate column x=9 by 1
;; rotate column x=8 by 2
;; rotate column x=7 by 1
;; rotate column x=4 by 1
;; rotate column x=3 by 1
;; rotate column x=2 by 1
;; rotate column x=0 by 1
;; rect 34x1
;; rotate column x=44 by 1
;; rotate column x=24 by 1
;; rotate column x=19 by 1
;; rotate row y=1 by 8
;; rotate row y=0 by 10
;; rotate column x=8 by 1
;; rotate column x=7 by 1
;; rotate column x=6 by 1
;; rotate column x=5 by 2
;; rotate column x=3 by 1
;; rotate column x=2 by 1
;; rotate column x=1 by 1
;; rotate column x=0 by 1
;; rect 9x1
;; rotate row y=0 by 40
;; rotate column x=43 by 1
;; rotate row y=4 by 10
;; rotate row y=3 by 10
;; rotate row y=2 by 5
;; rotate row y=1 by 10
;; rotate row y=0 by 15
;; rotate column x=7 by 2
;; rotate column x=6 by 3
;; rotate column x=5 by 2
;; rotate column x=3 by 2
;; rotate column x=2 by 4
;; rotate column x=0 by 2
;; rect 9x2
;; rotate row y=3 by 47
;; rotate row y=0 by 10
;; rotate column x=42 by 3
;; rotate column x=39 by 4
;; rotate column x=34 by 3
;; rotate column x=32 by 3
;; rotate column x=29 by 3
;; rotate column x=22 by 3
;; rotate column x=19 by 3
;; rotate column x=14 by 4
;; rotate column x=4 by 3
;; rotate row y=4 by 3
;; rotate row y=3 by 8
;; rotate row y=1 by 5
;; rotate column x=2 by 3
;; rotate column x=1 by 3
;; rotate column x=0 by 2
;; rect 3x2
;; rotate row y=4 by 8
;; rotate column x=45 by 1
;; rotate column x=40 by 5
;; rotate column x=26 by 3
;; rotate column x=25 by 5
;; rotate column x=15 by 5
;; rotate column x=10 by 5
;; rotate column x=7 by 5
;; rotate row y=5 by 35
;; rotate row y=4 by 42
;; rotate row y=2 by 5
;; rotate row y=1 by 20
;; rotate row y=0 by 45
;; rotate column x=48 by 5
;; rotate column x=47 by 5
;; rotate column x=46 by 5
;; rotate column x=43 by 5
;; rotate column x=41 by 5
;; rotate column x=38 by 5
;; rotate column x=37 by 5
;; rotate column x=36 by 5
;; rotate column x=33 by 1
;; rotate column x=32 by 5
;; rotate column x=31 by 5
;; rotate column x=30 by 1
;; rotate column x=28 by 5
;; rotate column x=27 by 5
;; rotate column x=26 by 5
;; rotate column x=23 by 1
;; rotate column x=22 by 5
;; rotate column x=21 by 5
;; rotate column x=20 by 1
;; rotate column x=17 by 5
;; rotate column x=16 by 5
;; rotate column x=13 by 1
;; rotate column x=12 by 3
;; rotate column x=7 by 5
;; rotate column x=6 by 5
;; rotate column x=3 by 1
;; rotate column x=2 by 3

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl))

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
