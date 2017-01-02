;;; day17 --- Solving day 17 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: yjjvjgan

;;; Code:

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/predictive")
(require 'queue)

(defconst q-x-dim 4)
(defconst q-y-dim 4)

(defvar *q-key* nil)

(defun q-can-move-p (coord dir)
  "Produce t if you can move from COORD in the direction of DIR."
  (pcase coord
    (`(,x . ,y)
     (pcase dir
       ('U (/= 0 y))
       ('D (/= (1- q-y-dim) y))
       ('L (/= 0 x))
       ('R (/= (1- q-x-dim) x))))))

(defun q-path-to-string (path)
  "Produce a string representation of PATH."
  (apply #'concat (mapcar #'symbol-name path)))

(defun q-open-doors (path)
  "Produce a list of the doors which are open given PATH."
  (let ((hash (secure-hash 'md5 (concat *q-key* (q-path-to-string path))))
        doors)
    (when (> (aref hash 0) ?a)
      (push 'U doors))
    (when (> (aref hash 1) ?a)
      (push 'D doors))
    (when (> (aref hash 2) ?a)
      (push 'L doors))
    (when (> (aref hash 3) ?a)
      (push 'R doors))
    doors))

(defun q-move (coord dir path)
  "Move from COORD in DIR and track with PATH."
  (pcase coord
    (`(,x . ,y)
     (pcase dir
       ('U (cons (cons x (1- y)) (append path '(U))))
       ('D (cons (cons x (1+ y)) (append path '(D))))
       ('L (cons (cons (1- x) y) (append path '(L))))
       ('R (cons (cons (1+ x) y) (append path '(R))))))))

(defun q-generate-moves (coord path)
  "Produce a list of the new positions from COORD having taken PATH."
  (let* ((open-doors (q-open-doors path))
         (real-doors (cl-remove-if-not (lambda (door) (q-can-move-p coord door)) open-doors)))
    (mapcar (lambda (door) (q-move coord door path)) real-doors)))

(defun q-done-p (coord)
  "Produce t if COORD is `done'."
  (pcase coord
    (`(,x . ,y)
     (and (= x (1- q-x-dim))
          (= y (1- q-y-dim))))))

(defun q-peek (queue)
  "Produce the first element of QUEUE without modifying the queue."
  (car (queue-head queue)))

(defun q-shortest-path (key)
  "Produce the shortest path from (0, 0) to the vault.

KEY is used in deteremining which door is open.
The vault is at (`*q-x-dim' - 1, `q-y-dim' - 1)."
  (let ((paths   (make-queue))
        (*q-key* key))
    (queue-enqueue paths (cons (cons 0 0) nil))
    (while (not (q-done-p (car (q-peek paths))))
      (let ((next-position (queue-dequeue paths)))
        (mapc (lambda (position) (queue-enqueue paths position))
              (q-generate-moves (car next-position)
                                (cdr next-position)))))
    (q-path-to-string (cdr (q-peek paths)))))

(defun q-longest-path (key)
  "Produce the longest path from (0, 0) to the vault.

KEY is used in deteremining which door is open.
The vault is at (`*q-x-dim' - 1, `q-y-dim' - 1)."
  (let ((paths   (make-queue))
        (*q-key* key)
        longest)
    (queue-enqueue paths (cons (cons 0 0) nil))
    (while (not (queue-empty paths))
      (let ((next-position (queue-dequeue paths)))
        (if (and (q-done-p (car next-position))
                 (> (length (cdr next-position)) (length longest)))
            (setq longest (cdr next-position))
          (mapc (lambda (position) (queue-enqueue paths position))
                (q-generate-moves (car next-position)
                                  (cdr next-position))))))
    (q-path-to-string longest)))

(defun q-longest-path-df-iter (position)
  "Find the length of the longest path from POSITION to the vault."
  (if (q-done-p   (car position))
      (length     (cdr position))
    (let ((generated (q-generate-moves (car position)
                                       (cdr position)))
          max-length)
      (while generated
        (let* ((next        (pop generated))
               (next-length (q-longest-path-df-iter next)))
          (when (and (not (null next-length))
                     (or (null max-length)
                         (> next-length max-length)))
            (setq max-length next-length))))
      max-length)))

(defun q-longest-path-df (key)
  "Produce the length of the longest path from (0, 0) to the vault.

KEY is used in deteremining which door is open.
The vault is at (`*q-x-dim' - 1, `q-y-dim' - 1)."
  (let ((*q-key*             key)
        (max-lisp-eval-depth 1000000))
    (q-longest-path-df-iter (cons (cons 0 0) nil))))

(defun q-answer-question (input)
  "Find the shortest path to the vault using the key INPUT."
  (interactive "sKey: ")
  (if (y-or-n-p "Find the length of the longest? ")
      (message "%s" (q-longest-path-df input))
    (message "%s" (q-shortest-path input))))

;; Shortest path:     RLDRUDRDDR
;; Length of longest: 498

(provide 'day17)
;;; day17 ends here
