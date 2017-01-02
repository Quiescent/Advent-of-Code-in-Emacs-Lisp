;;; day15 --- solving day 15 -*- lexical-binding: t; -*-

;;; Commentary:

;; Position zero is clear for the capsule to fall through.

;; My input:
;; Disc #1 has 13 positions; at time=0, it is at position 11.
;; Disc #2 has 5 positions; at time=0, it is at position 0.
;; Disc #3 has 17 positions; at time=0, it is at position 11.
;; Disc #4 has 3 positions; at time=0, it is at position 0.
;; Disc #5 has 7 positions; at time=0, it is at position 2.
;; Disc #6 has 19 positions; at time=0, it is at position 17.

;; A new disc was added for part two, but not included in the input.
;; I'm adding the following line when I run it.

;; "but a new disc with 11 positions and starting at position 0"
;; -> Disc #7 has 11 positions; at time=0, it is at position 0.

;; Making the input
;; Disc #1 has 13 positions; at time=0, it is at position 11.
;; Disc #2 has 5 positions; at time=0, it is at position 0.
;; Disc #3 has 17 positions; at time=0, it is at position 11.
;; Disc #4 has 3 positions; at time=0, it is at position 0.
;; Disc #5 has 7 positions; at time=0, it is at position 2.
;; Disc #6 has 19 positions; at time=0, it is at position 17.
;; Disc #7 has 11 positions; at time=0, it is at position 0.

;;; Code:

(require 'cl)

(defun q-parse-line (line)
  "Parse LINE into a disc."
  (pcase (split-string line " " t)
    (`("Disc" ,num "has" ,positions "positions;" "at" ,time "it" "is" "at" "position" ,starting)
     `(DISC ,(string-to-number (cadr (split-string num "#")))
            ,(string-to-number positions)
            ,(string-to-number (cadr (split-string time "=" ",")))
            ,(string-to-number starting)))))

(defun q-parse (input)
  "Parse INPUT into an array representing the positions of 4 discs."
  (mapcar #'q-parse-line (split-string input "\n" t "\\.")))

(defun q-increasing-nums (xs)
  "Produce a sequence of increasing integers, starting at 1 for each x in XS."
  (let (result)
    (dotimes (i (length xs) result)
      (setq result (nconc result (list (1+ i)))))))

(defun q-advance-disc (disc i)
  "Advance position of DISC by I."
  (pcase disc
    (`(DISC ,num ,positions ,time ,position)
     `(DISC ,num ,positions ,(+ time i) ,(mod (+ position i) positions)))))

(defun q-would-pass-through (discs)
  "Produce t if a capsule would pass through DISCS in their current configuration."
  ;; Should renormalise into time=0 here, but since the data set has
  ;; them in t=0 I'm going to skip that for now.
  (cl-every (pcase-lambda (`(DISC ,_ ,_ ,_ ,position)) (= position 0))
            (mapcar* (pcase-lambda (disc advance-by)
                       (q-advance-disc disc advance-by))
                     discs
                     (q-increasing-nums discs))))

(defun q-advance-by-second (discs)
  "Advance the position of each disc in DISCS by one second."
  (mapcar (lambda (disc) (q-advance-disc disc 1)) discs))

(defun q-first-gap (discs)
  "Produce the time to wait before dropping a capsule through DISCS.

Capsule it dropped so that it passes through all the slots."
  (let ((delay 0))
    (while (not (q-would-pass-through discs))
      (setq discs (q-advance-by-second discs))
      (cl-incf delay))
    delay))

(defun q-answer-question (input)
  "Find the time to wait before dropping the capsule given discs in INPUT."
  (interactive "sInput: ")
  (let ((discs (q-parse input)))
    (message "%s" (q-first-gap discs))))

;; Solution 1: 122318
;; Solution 2: 3208583

(provide 'day15)
;;; day15 ends here
