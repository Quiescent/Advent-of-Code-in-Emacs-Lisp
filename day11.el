;;; day11 --- Solving day 11 -*- lexical-binding: t; -*-

;;; Commentary:

;; Descriptions are of the format:
;; The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
;; The second floor contains a hydrogen generator.
;; The third floor contains a lithium generator.
;; The fourth floor contains nothing relevant.

;; This code assumes that there are as many RTGs as CHIPs (important
;; for things like checking validity which can make assumptions about
;; the symmetry of bits in rows because we sorted by type -CHIPs < RTGs-
;; type then subtype before encoding.)

;; Input for part one:
;; The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
;; The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
;; The third floor contains a thulium-compatible microchip.
;; The fourth floor contains nothing relevant.

;; There wasn't a valid input with the extra components.  So i've written one.
;; The components added were:
;;  - An elerium generator.
;;  - An elerium-compatible microchip.
;;  - A dilithium generator.
;;  - A dilithium-compatible microchip.
;; The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, a elerium generator, a elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip and a plutonium-compatible microchip.
;; The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
;; The third floor contains a thulium-compatible microchip.
;; The fourth floor contains nothing relevant.

;;; Code:

(require 'seq)
(add-to-list 'load-path "~/.emacs.d/predictive")
(require 'queue)
(require 'cl)

(defconst q-line-start-sentinal "contains")
(defconst q-len-sentinal        (length q-line-start-sentinal))

(defvar *q-top-floor*     nil)
(defvar *q-item-count*    nil)
(defvar *q-done-board*    nil)
(defvar *q-half-len*      nil)
(defvar *q-rtg-bitmask*   nil)
(defvar *q-chip-bitmask*  nil)
(defvar *q-max-bit*       nil)
(defvar *q-floor-limiter* nil)

(defun q-parse-item (item)
  "Parse an RTG or CHIP from ITEM."
  (pcase (split-string item " " t "\\.")
    (`("a" ,chip-type "microchip")
     `(CHIP . ,(intern (car (split-string chip-type "-" t " ")))))
    (`("a" ,rtg-type "generator")
     `(RTG . ,(intern rtg-type)))))

(defun q-parse-line (line)
  "Parse LINE into a list of the RTGs and CHIPs on it."
  (mapcar #'q-parse-item
          (split-string (substring line
                                   (+ q-len-sentinal (string-match q-line-start-sentinal
                                                                   line)))
                        ", \\|and " t " ")))

(defun q-parse (input)
  "Parse INPUT into a map containing the locations of RTGs and CHIPs."
  (mapcar #'q-parse-line (split-string input "\n" t " ")))

(defun q-encoding (xs)
  "Produce an encoding in the form of an alist for XS."
  (let ((curr-code 1))
    (cons (cons (car xs) curr-code)
          (mapcar (lambda (x)
                    (cons x (setq curr-code (lsh curr-code 1))))
                  (cdr xs)))))

(defun q-element-encoding (board)
  "Produce an encoding as an alist for BOARD when sorted by type, then subtype."
  (q-encoding
   (sort (cl-remove-if #'null (apply #'append board))
         (pcase-lambda (`(,this-type . ,this-subtype)
                        `(,that-type . ,that-subtype))
           (or (string< (symbol-name this-type) (symbol-name that-type))
               (and (string= (symbol-name this-type) (symbol-name that-type))
                    (string< (symbol-name this-subtype) (symbol-name that-subtype))))))))

(defun q-or (xs)
  "Apply or to XS."
  (let ((result (car xs)))
    (dolist (x xs result)
      (setq result (logior x result)))))

(defun q-encode-board (board encoding)
  "Produce a binary encoded version of BOARD using ENCODING (an alist)."
  (apply #'vector
         (mapcar (lambda (row)
                   (let ((encoded-row (mapcar (lambda (elt) (alist-get elt encoding)) row)))
                     (if (car encoded-row)
                         (q-or encoded-row)
                       0)))
                 board)))

(defun q-row-put (x board row)
  "Put X into the BOARD at ROW."
  (progn
    (aset board row (logior x (aref board row)))
    board))

(defun q-row-kill (x board row)
  "Remove X from the BOARD at ROW."
  (progn
    (aset board row (logand (lognot x) (aref board row)))
    board))

(defun q-done (board)
  "Produce t if BOARD is in the done state."
  (equal board *q-done-board*))

(defun q-valid-row (row)
  "Produce t if the ROW is valid."
  (let* ((chips (logand *q-chip-bitmask* row))
         (rtgs  (logand *q-rtg-bitmask*  row)))
    ;; There are no RTGS
    (or (= 0 rtgs)
        ;; OR there is an RTG for every chip
        (= chips (logand (lsh rtgs (- 0 *q-half-len*)) chips)))))

(defun q-generate-boards (board floor)
  "Generate all valid boards from BOARD when on FLOOR."
  (let ((boards        '())
        (i             1))
    (cl-labels ((maybe-cons (elt cdr)
                            (if (cdr elt)
                                (cons elt cdr)
                              cdr))
                (mapcons (x ys) (mapcar (lambda (y) (cons x y)) ys))
                (move-bit
                 (pos row target board)
                 (q-row-put pos (q-row-kill pos (seq-subseq board 0) row) target))
                (q-generate-1
                 (board i target)
                 (let ((this-floor (aref board floor))
                       result)
                   (while (/= *q-max-bit* i)
                     (when (/= 0 (logand i this-floor))
                       (let ((next-board (move-bit i floor target board)))
                         (when (q-valid-row (aref next-board target))
                           (push next-board result))))
                     (setq i (lsh i 1)))
                   result)))
      (while (/= i *q-max-bit*)
        (when (/= 0 (logand i   (aref board floor)))
          (let* ((next-i        (lsh i 1))
                 (target-up     (1+ floor))
                 (target-down   (1- floor))
                 (can-move-up   (/= floor *q-floor-limiter*))
                 (move-up       (when can-move-up (move-bit i floor target-up board)))
                 (can-move-down (/= floor 0))
                 (move-down     (when can-move-down (move-bit i floor target-down board)))
                 (up-valid      (and can-move-up   (q-valid-row (aref move-up   target-up))))
                 (down-valid    (and can-move-down (q-valid-row (aref move-down target-down)))))
            (setq boards
                  (nconc boards
                         (maybe-cons (cons target-up (when up-valid move-up))
                                     (maybe-cons (cons target-down (when down-valid move-down))
                                                 (when can-move-up
                                                   (mapcons target-up
                                                            (q-generate-1 (seq-subseq move-up 0)
                                                                          next-i target-up)))))
                         (when can-move-down
                           (mapcons target-down
                                    (q-generate-1 (seq-subseq move-down 0) next-i target-down)))))))
        (setq i (lsh i 1)))
      boards)))

(defun q-peek (queue)
  "Produce the first element of QUEUE without dequeueing it."
  (car (queue-head queue)))


(defun q-perfect-hash-key (floor positions)
  "Create a key for a hash table from FLOOR and POSITIONS.

This function is guaranteed not to collide and has an ordering."
  (let ((val floor)
        (i   1))
    (seq-do (lambda (x)
              (progn
                (setq val (+ val (* x i))
                      i   (* i *q-top-floor*))))
            positions)
    val))

(defun q-cache-put (board cache)
  "Put t to the association of BOARD from CACHE."
  (aset cache (q-perfect-hash-key (car board) (cdr board)) t))

(defun q-cache-get (board cache)
  "Produce the assocation of BOARD from CACHE."
  (aref cache (q-perfect-hash-key (car board) (cdr board))))

(defun q-search (board)
  "Produce the number of movements to get BOARD to a `done' state."
  (let ((boards (queue-create))
        (seen   (make-vector (1+ (q-perfect-hash-key (1- *q-top-floor*) *q-done-board*)) nil)))
    (q-cache-put (cons 0 board) seen)
    (queue-enqueue boards (cons 0 (cons 0 board)))
    (while (not (q-done (cddr (q-peek boards))))
      (let* ((head (queue-dequeue boards)))
        (mapc (lambda (board)
                (when (not (q-cache-get board seen))
                  (queue-enqueue boards (cons (1+ (car head)) board))
                  (q-cache-put board seen)))
              (q-generate-boards (cddr head) (cadr head)))))
    (car (q-peek boards))))

(defun q-create-done-board ()
  "Create a `done' board from the floors `*q-top-floor*' & `*q-item-count*'."
  (let ((board '()))
    (dotimes (_ (1- *q-top-floor*))
      (push 0 board))
    (setq board (nconc board `(,(1- (lsh 1 *q-item-count*)))))
    (apply #'vector board)))

(defun q-answer-question (input)
  "Find the minimum number of movements to the solution from INPUT."
  (interactive "sInput: ")
  (let* ((start             (current-time))
         (*q-top-floor*     4)
         (board             (q-parse input))
         (encoding          (q-element-encoding board))
         (*q-item-count*    (length encoding))
         (*q-done-board*    (q-create-done-board))
         (*q-half-len*      (/ *q-item-count* 2))
         (*q-rtg-bitmask*   (logxor (1- (lsh 1 *q-item-count*))
                                    (1- (lsh 1 *q-half-len*))))
         (*q-chip-bitmask*  (lognot *q-rtg-bitmask*))
         (*q-max-bit*       (lsh 1 *q-item-count*))
         (*q-floor-limiter* (1- *q-top-floor*)))
    (message "%s in %s seconds (queue)" (q-search (q-encode-board board encoding))
             (format-time-string "%s" (time-since start)))))

;; First part:  37
;; Second part: 61

(provide 'day11)
;;; day11 ends here
