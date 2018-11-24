;;; day7-loop --- My solution to day7-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; My solution to advent of code: day7-loop

;;; Code:

;; # PART 1:

(require 'cl-lib)

(defun abba-p (str i)
  "Produce t if STR is an ABBA from index I."
  (and (<= i (- (length str) 4))
       (eq (aref str i)
           (aref str (+ i 3)))
       (eq (aref str (1+ i))
           (aref str (+ i 2)))
       (not (eq (aref str (1+ i))
                (aref str i)))))

(defun parse-address (address)
  "Produce the hyper and non hypernet sequences of ADDRESS."
  (let ((hypers     '())
        (non-hypers '())
        (in-hyper   nil))
    (with-temp-buffer
      (insert (concat address "["))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (search-forward (if in-hyper "]" "["))
          (backward-char)
          (push (buffer-substring start (point))
                (if in-hyper hypers non-hypers))
          (setq in-hyper (not in-hyper))
          (ignore-errors (forward-char 1))))
      `(,non-hypers ,hypers))))

(defun abba-anywhere (str)
  "Produce t if there's an abba anywhere in STR."
  (cl-loop for i from 0 below (length str)
     thereis (abba-p str i)))

(defun day7-loop-part-1 (input-file)
  "Run my solution to part one of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     for (non-hypers hypers) = (parse-address line)
     count (and (cl-some #'abba-anywhere non-hypers)
                (not (cl-some #'abba-anywhere hypers)))))

;; # PART 2:

(defun aba (str i)
  "Produce an aba if STR is an ABA from index I."
  (and (<= i (- (length str) 3))
       (eq (aref str i)
           (aref str (+ i 2)))
       (not (eq (aref str (1+ i))
                (aref str i)))
       (substring str i (+ i 3))))

(defun aba-to-bab (str)
  "Produce the BAB of ABA STR."
  (string (aref str 1)
          (aref str 0)
          (aref str 1)))

(defun all-abas (str)
  "Produce all of the aba's in STR."
  (cl-loop for i from 0 below (length str)
     for current-aba = (aba str i)
     when current-aba
       collect current-aba))

(defun day7-loop-part-2 (input-file)
  "Run my solution to part two of the problem on the input in INPUT-FILE."
  (cl-loop for line in (split-string input-file "\n" t " ")
     for (non-hypers hypers) = (parse-address line)
     for abas = (apply #'append (mapcar #'all-abas non-hypers))
     for hyper-abas = (apply #'append (mapcar #'all-abas hypers))
     count (cl-some (lambda (an-aba) (let ((a-bab (aba-to-bab an-aba)))
                                       (member a-bab hyper-abas)))
              abas)))

;; Run the solution:

(progn
  (message "\n********** OUTPUT **********")
  (let ((input-1 (with-temp-buffer
                   (find-file-literally "day7-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max))))
        (input-2 (with-temp-buffer
                   (find-file-literally "day7-loop-part-1")
                   (buffer-substring (point-min)
                                     (point-max)))))
    (message "Part 1: %s" (day7-loop-part-1 input-1))
    (message "Part 2: %s\n" (day7-loop-part-2 input-2))))

(provide 'day7-loop)
;;; day7-loop ends here
