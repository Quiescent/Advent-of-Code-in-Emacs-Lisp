;;; day19 --- Solving day 19 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: 3005290

;;; Code:

(require 'ring)

(defun q-grab-tale (xs)
  "Make the last element in XS point to the first."
  (let ((ys xs)
        end)
    (while ys
      (setq end ys
            ys  (cdr ys)))
    (setcdr end xs)))

(defun q-make-ascending-list (count)
  "Create a list of numbers from 1 up to COUNT."
  (let (result)
    (q-grab-tale (nreverse (dotimes (i count result)
                             (push (1+ i) result))))))

(defun q-answer-question-list-ring (elf-count)
  "Produce the number of the elf (starting from 1) who gets all the presents.

There are ELF-COUNT elves."
  (interactive "nElf Count: ")
  (let* ((elves       (q-make-ascending-list elf-count))
         (elves-rem   elf-count)
         (elf-pt      elves)
         (curr-elf    elves)
         (skip        (if (y-or-n-p "Exchange with oposing elf? ")
                          (lambda () (floor (/ elves-rem 2)))
                        (lambda () 1))))
    (cl-labels ((rem-at (x)
                        (dotimes (_ (1- x))
                          (setq elf-pt (cdr elf-pt)))
                        (let ((end elf-pt))
                          (dotimes (_ (1- x))
                            (setq end (cdr end)))
                          (setcdr elf-pt (cdr end))
                          (setq elf-pt end))))
      (while (/= 1 elves-rem)
        (let ((curr-skip (funcall skip)))
          (rem-at curr-skip)
          (setq curr-elf elf-pt)
          (cl-incf elves-rem (- 0 curr-skip)))))
    (message "%s" (car curr-elf))))

(defun q-make-ascending-ring (count)
  "Create a ring with the numbers [1, COUNT] in it."
  (let ((ring (make-ring count)))
    (dotimes (idx count ring)
      (ring-insert ring (- count idx)))))

(defun q-answer-question-ring (elf-count)
  "Produce the number of the elf (starting from 1) who gets all the presents.

There are ELF-COUNT elves."
  (interactive "nElf Count: ")
  (let* ((elves       (q-make-ascending-ring elf-count))
         (elves-rem   elf-count)
         (skip        (if (y-or-n-p "Exchange with oposing elf? ")
                          (lambda () (floor (/ elves-rem 2)))
                        (lambda () 1)))
         (current-elf 0)
         (next-elf    (funcall skip)))
    (while (/= 1 elves-rem)
      (ring-remove elves next-elf)
      (setq current-elf (mod (1+ current-elf)
                             elves-rem))
      (cl-incf elves-rem -1)
      (setq next-elf    (mod (+ current-elf (funcall skip))
                             elves-rem)))
    (message "%s" (ring-ref elves current-elf))))

(defun q-answer-question (elf-count)
  "Produce the number of the elf (starting from 1) who gets all the presents.

There are ELF-COUNT elves."
  (interactive "nElf Count: ")
  (let* ((elves       (make-vector elf-count t))
         (elves-rem   elf-count)
         (skip        (if (y-or-n-p "Exchange with oposing elf? ")
                          (lambda () (floor (/ elves-rem 2)))
                        (lambda () 1))))
    (cl-labels ((skip-forward
                 (cnt pos)
                 (dotimes (_ cnt pos)
                   (setq pos (mod (1+ pos) elf-count))
                   (while (null (aref elves pos))
                     (setq pos (mod (1+ pos) elf-count))))))
      (let* ((current-elf 0)
             (next-elf    (skip-forward (funcall skip) current-elf)))
        (while (/= current-elf next-elf)
          (aset elves next-elf nil)
          (cl-incf elves-rem -1)
          (let ((this-skip (funcall skip)))
            (setq current-elf (skip-forward 1         current-elf))
            (setq next-elf    (skip-forward this-skip current-elf))))
        (message "%s" (1+ current-elf))))))

;; Part one: 1816277
;; Part two: 1410967

(provide 'day19)
;;; day19 ends here
