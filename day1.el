;;; day1 --- solving day one -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4

;;; Code:

(defvar *q-direction*      nil)
(defvar *q-pos*            nil)
(defvar *q-visited-so-far* nil)
(defvar *q-stop-at-dup*    nil)

(defun q-up (pos mag)
  "Produce POS, moved MAG times, updwards."
  (cons (car pos) (+ mag (cdr pos))))

(defun q-down (pos mag)
  "Produce POS, moved MAG times, downwards."
  (cons (car pos) (- (cdr pos) mag)))

(defun q-right (pos mag)
  "Produce POS, moved MAG times, right."
  (cons (+ mag (car pos)) (cdr pos)))

(defun q-left (pos mag)
  "Produce POS, moved MAG times, left."
  (cons (- (car pos) mag) (cdr pos)))

(defun q-go (instructions)
  "Go in the given INSTRUCTIONS and report the ending position."
  (while instructions
    (let* ((instruction (car instructions))
           (rotate      (car instruction))
           (mag         (cdr instruction)))
      (progn
        (cond
         ((eq rotate 'R) (setq *q-direction* (rotate--90 *q-direction*)))
         ((eq rotate 'L) (setq *q-direction* (rotate-+90 *q-direction*))))
        (while (> mag 0)
          (q-move 1)
          (if (and *q-stop-at-dup*
                   (member *q-pos* *q-visited-so-far*))
              (progn
                (setq instructions nil)
                (setq mag          0)))
          (push *q-pos* *q-visited-so-far*)
          (cl-decf mag))
        (setq instructions (cdr instructions))))))

(defun q-move (mag)
  "Move MAG steps in the current direction."
  (cond
   ((eq *q-direction* 'NORTH) (setq *q-pos* (q-up    *q-pos* mag)))
   ((eq *q-direction* 'WEST)  (setq *q-pos* (q-left  *q-pos* mag)))
   ((eq *q-direction* 'SOUTH) (setq *q-pos* (q-down  *q-pos* mag)))
   ((eq *q-direction* 'EAST)  (setq *q-pos* (q-right *q-pos* mag)))))

(defun rotate-+90 (direction)
  "Rotate DIRECTION 90 degrees positively."
  (cond
   ((eq direction 'NORTH) 'WEST)
   ((eq direction 'WEST)  'SOUTH)
   ((eq direction 'SOUTH) 'EAST)
   ((eq direction 'EAST)  'NORTH)))

(defun rotate--90 (direction)
  "Rotate DIRECTION 90 degrees negatively."
  (cond
   ((eq direction 'NORTH) 'EAST)
   ((eq direction 'EAST)  'SOUTH)
   ((eq direction 'SOUTH) 'WEST)
   ((eq direction 'WEST)  'NORTH)))

(defun q-parse-input (input)
  "Parse INPUT into a series of symbol, number dotted lists."
  (let (result)
    (cl-labels ((parse-r-or-l (x) (if (string= x "R") 'R (if (string= x "L") 'L nil)))
                (parse-one    (x) (cons (parse-r-or-l (substring x 0 1))
                                        (string-to-number (substring x 1)))))
      (nreverse
       (dolist (x (split-string input ", ") result)
         (push (parse-one x) result))))))

(defun q-answer-question (input)
  "Find shortest distance to the end of where INPUT directs you."
  (interactive "sInput:")
  (let* ((*q-direction*      'NORTH)
         (*q-pos*            (cons 0 0))
         (*q-visited-so-far* (list *q-pos*))
         (*q-stop-at-dup*    (y-or-n-p "Stop at first cross? "))
         (parsed (q-parse-input input)))
    (q-go parsed)
    (message "%s" (+ (abs (car *q-pos*)) (abs (cdr *q-pos*))))))

;; answer w/o stopping:   181
;; answer to stop at dup: 140

(provide 'day1)
;;; day1 ends here
