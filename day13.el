;;; day13 --- Solving day 13 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: 1350

;;; Code:

(eval-and-compile
  (add-to-list 'load-path "predictive"))
(require 'queue)
(require 'subr-x)

(defconst q-destination '(31 . 39))

(defvar *q-favourite-number*      nil)
(defvar *q-count-blocks-in-range* nil)
(defvar *q-range*                 nil)

(defun q-count-bits (x)
  "Count the number of bits in X."
  (let ((cnt 0))
    (while (> x 0)
      (cl-incf cnt (logand 1 x))
      (setq x (lsh x -1)))
    cnt))

(defun q-wall-p (x y fav)
  "Determine whether the coordinate (X, Y) is a wall given the favourite number FAV."
  (= 1 (mod (q-count-bits (+ fav (* x x) (* 3 x) (* 2 x y) y (* y y))) 2)))

(defun q-queue-first (queue)
  "Produce the next element of QUEUE without dequeueing."
  (car (queue-head queue)))

(defun q-head-done (queue)
  "Produce t if the next element in QUEUE is the final location."
  (equal q-destination (cdr (q-queue-first queue))))

(defun q-positive-location (location)
  "Produce t if LOCATION is in bounds."
  (pcase location
    (`(,x . ,y)
     (and (>= x 0)
          (>= y 0)))))

(defun q-generate-locations (location)
  "Generate all locations from LOCATION."
  (pcase location
    (`(,cnt . (,x . ,y))
     (mapcar (lambda (x) (cons (1+ cnt) x))
             (cl-remove-if (pcase-lambda (`(,this-x . ,this-y)) (q-wall-p this-x this-y *q-favourite-number*))
                           (cl-remove-if-not #'q-positive-location
                                             (list (cons (1- x) y)
                                                   (cons (1+ x) y)
                                                   (cons x      (1+ y))
                                                   (cons x      (1- y)))))))))

(defun q-shortest-path ()
  "Find the shortest number of movements from the current location to the current destination."
  (let* ((current-locations (queue-create))
         (locations-seen    (make-hash-table :test #'equal))
         (done-test         (if *q-count-blocks-in-range*
                                (lambda () (not (= 0 (queue-length current-locations))))
                              (lambda () (not (q-head-done current-locations))))))
    (progn
      (queue-enqueue current-locations (cons 0 (cons 1 1)))
      (puthash (cons 1 1) (cons 1 1) locations-seen)
      (cl-labels ((remove-and-record (xs)
                                     (cl-remove-if-not (pcase-lambda (`(,cnt . ,x)) (progn (if (or (and *q-count-blocks-in-range*
                                                                                                        (> cnt *q-range*))
                                                                                                   (gethash x locations-seen))
                                                                                               nil
                                                                                             (puthash x cnt locations-seen))))
                                                       xs)))
        (while (funcall done-test)
          (mapc (lambda (x) (queue-enqueue current-locations x))
                (remove-and-record (q-generate-locations (queue-dequeue current-locations)))))
        (if *q-count-blocks-in-range*
            locations-seen
          (q-queue-first current-locations))))))

(defun q-answer-question (fav)
  "Computer the shortest steps to `q-destination' from (1, 1) given FAV."
  (interactive "nFavourite number: ")
  (let* ((*q-favourite-number* fav)
         (*q-range* 50)
         (*q-count-blocks-in-range* (y-or-n-p "Count blocks in range? "))
         (ans (q-shortest-path)))
    (if *q-count-blocks-in-range*
        (message "%s" (length (hash-table-keys ans)))
      (message "%s" (car ans)))))

;; First part (1350): 92
;; Part two (1350):   124

(provide 'day13)
;;; day13 ends here
