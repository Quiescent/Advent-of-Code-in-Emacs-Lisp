;;; day2-loop --- My solution to day2-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day2-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defconst grid [[1 2 3] [4 5 6] [7 8 9]]
  "The simply keypad grid.")

(defun day2-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     named line-loop
     with x = 1
     with y = 1
     for line in (split-string input-file "\n" t " ")
     do (cl-loop for button across line
           do (cl-incf x (pcase button
                           ('?L -1)
                           ('?U 0)
                           ('?D 0)
                           ('?R 1)))
           do (setq x (if (< x 0) 0 (if (> x 2) 2 x)))
           do (cl-incf y (pcase button
                           ('?L 0)
                           ('?U -1)
                           ('?D 1)
                           ('?R 0)))
           do (setq y (if (< y 0) 0 (if (> y 2) 2 y))))
     collect (aref (aref grid y) x)))

;; # PART 2:

;;     1
;;   2 3 4
;; 5 6 7 8 9
;;   A B C
;;     D

(defconst fancy-grid [[?1] [?2 ?3 ?4] [?5 ?6 ?7 ?8 ?9] [?A ?B ?C] [?D]]
  "The fancy keypad grid.")

(defun day2-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     named line-loop
     with key = ?5
     for line in (split-string input-file "\n" t " ")
     do (cl-loop for direction across line
           do (setq key
                    (pcase `(,key ,direction)
                      ('(?1 ?D) ?3)

                      ('(?2 ?R) ?3)
                      ('(?2 ?D) ?6)

                      ('(?3 ?L) ?2)
                      ('(?3 ?U) ?1)
                      ('(?3 ?R) ?4)
                      ('(?3 ?D) ?7)

                      ('(?4 ?L) ?3)
                      ('(?4 ?D) ?8)

                      ('(?5 ?R) ?6)

                      ('(?6 ?L) ?5)
                      ('(?6 ?U) ?2)
                      ('(?6 ?R) ?7)
                      ('(?6 ?D) ?A)

                      ('(?7 ?L) ?6)
                      ('(?7 ?U) ?3)
                      ('(?7 ?R) ?8)
                      ('(?7 ?D) ?B)

                      ('(?8 ?L) ?7)
                      ('(?8 ?U) ?4)
                      ('(?8 ?R) ?9)
                      ('(?8 ?D) ?C)

                      ('(?9 ?L) ?8)

                      ('(?A ?U) ?6)
                      ('(?A ?R) ?B)

                      ('(?B ?L) ?A)
                      ('(?B ?U) ?7)
                      ('(?B ?R) ?C)
                      ('(?B ?D) ?D)

                      ('(?C ?L) ?B)
                      ('(?C ?U) ?8)

                      ('(?D ?U) ?B)

                      (_ key))))
     concat (char-to-string key)))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day2-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day2-loop-part-1 input-1))
    (message "Part 2: %s\n" (day2-loop-part-2 input-2))))

(provide 'day2-loop)
;;; day2-loop ends here
