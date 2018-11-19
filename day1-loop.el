;;; day1-loop --- My solution to day1-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day1-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day1-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with x = 0
     with y = 0
     with direction = 'NORTH
     for instruction in (split-string input-file ", " t " ")
     for turn = (aref instruction 0)
     for amount-forward = (string-to-number (substring instruction 1))
     if (eq turn ?L)
       do (setq direction
                (pcase direction
                  ('NORTH 'WEST)
                  ('WEST  'SOUTH)
                  ('SOUTH 'EAST)
                  ('EAST  'NORTH)))
     else
       do (setq direction
                (pcase direction
                  ('NORTH 'EAST)
                  ('EAST  'SOUTH)
                  ('SOUTH 'WEST)
                  ('WEST  'NORTH)))
     do (cl-incf x (pcase direction
                     ('NORTH amount-forward)
                     ('EAST  0)
                     ('SOUTH (- 0 amount-forward))
                     ('WEST  0)))
     do (cl-incf y (pcase direction
                     ('NORTH 0)
                     ('EAST  (- 0 amount-forward))
                     ('SOUTH 0)
                     ('WEST  amount-forward)))
     finally return (+ (abs x) (abs y))))

;; # PART 2:

(defun day1-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     named direction-loop
     with x = 0
     with y = 0
     with locations-visited = `(,(cons 0 0))
     with direction = 'NORTH
     for instruction in (split-string input-file ", " t " ")
     for turn = (aref instruction 0)
     for amount-forward = (string-to-number (substring instruction 1))
     if (eq turn ?L)
       do (setq direction
                (pcase direction
                  ('NORTH 'WEST)
                  ('WEST  'SOUTH)
                  ('SOUTH 'EAST)
                  ('EAST  'NORTH)))
     else
       do (setq direction
                (pcase direction
                  ('NORTH 'EAST)
                  ('EAST  'SOUTH)
                  ('SOUTH 'WEST)
                  ('WEST  'NORTH)))
     if (member direction '(NORTH SOUTH))
       do (cl-loop
             for i below amount-forward
             do (cl-incf x (pcase direction
                             ('NORTH 1)
                             ('EAST  0)
                             ('SOUTH -1)
                             ('WEST  0)))
             when (member (cons x y) locations-visited)
               do (cl-return-from direction-loop (+ (abs x) (abs y)))
             do (push (cons x y) locations-visited))
     else
       do (cl-loop
             for i below amount-forward
             do (cl-incf y (pcase direction
                             ('NORTH 0)
                             ('EAST  -1)
                             ('SOUTH 0)
                             ('WEST  1)))
             when (member (cons x y) locations-visited)
               do (cl-return-from direction-loop (+ (abs x) (abs y)))
             do (push (cons x y) locations-visited))))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day1-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day1-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day1-loop-part-1 input-1))
    (message "Part 2: %s\n" (day1-loop-part-2 input-2))))

(provide 'day1-loop)
;;; day1-loop ends here
