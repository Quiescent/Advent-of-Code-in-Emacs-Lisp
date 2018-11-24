;;; day5-loop --- My solution to day5-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day5-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun day5-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop
     with password = [nil nil nil nil nil nil nil nil]
     with chars-so-far = 0
     with x = 0
     for next-hash = (md5 (concat input-file (number-to-string x)))
     do (cl-incf x)
     when (cl-loop
             for i from 0 to 4
             always (eq ?0 (aref next-hash i)))
       do (progn
            (aset password
                  chars-so-far
                  (char-to-string (aref next-hash 5)))
            (cl-incf chars-so-far))
     until (eq chars-so-far 8)
     finally return password))

;; # PART 2:

(require 'seq)

(defun day5-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop
     with password = [nil nil nil nil nil nil nil nil]
     with chars-so-far = 0
     with x = 0
     for next-hash = (md5 (concat input-file (number-to-string x)))
     do (cl-incf x)
     when (cl-loop
             for i from 0 to 4
             always (eq ?0 (aref next-hash i)))
       do (progn
            (let* ((idx-char (aref next-hash 5))
                   (idx-str  (char-to-string idx-char))
                   (idx      (string-to-number idx-str)))
              (when (and (<= idx-char ?7)
                         (>= idx-char ?0)
                         (not (aref password idx)))
                (aset password
                      idx
                      (char-to-string (aref next-hash 6)))
                (cl-incf chars-so-far))))
     until (eq chars-so-far 8)
     finally return password))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day5-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day5-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day5-loop-part-1 input-1))
    (message "Part 2: %s\n" (day5-loop-part-2 input-2))))

(provide 'day5-loop)
;;; day5-loop ends here
