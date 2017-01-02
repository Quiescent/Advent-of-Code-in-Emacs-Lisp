;;; day16 --- Solving day 16 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: 10111100110001111

;;; Code:

(require 'map)
(require 'cl)

(defconst q-disk-length        272)
(defconst q-long-disk-length 35651584)

(defun q-parse (input)
  "Parse a vector of zeros and ones from the given INPUT."
  (apply #'vector (mapcar #'string-to-number (split-string input "" t))))

(defun q-generate-one (data)
  "Generate an extended stretch of random data from DATA."
  (let ((a (seq-map #'identity data))
        (b (seq-map (lambda (x) (if (= 0 x) 1 0)) (seq-reverse data))))
    (apply #'vector (append a '(0) b))))

(defun q-generate-data (length initial)
  "Generate data to fill /up to/ LENGTH bits from INITIAL."
  (let ((output (q-generate-one initial)))
    (while (< (length output) length)
      (setq output (q-generate-one output)))
    (seq-subseq output 0 length)))

(defun q-checksum (data)
  "Compute the checksum of DATA."
  (let ((new-data (make-vector (/ (length data) 2) 0)))
    (while (= 0 (mod (length data) 2))
      (dotimes (i (/ (length data) 2))
        (let ((pos (* 2 i)))
          (aset new-data i (if (= (aref data pos) (aref data (1+ pos))) 1 0))))
      (setq data new-data)
      (setq new-data (make-vector (/ (length data) 2) 0)))
    data))

(defun q-answer-question (input)
  "Compute the checksum of generating randomish data from INPUT."
  (interactive "sInput: ")
  (let ((parsed (q-parse input))
        (length (if (y-or-n-p "Use long disk? ")
                    q-long-disk-length
                  q-disk-length)))
    (message "%s" (apply #'string
                         (seq-map (lambda (x) (aref (number-to-string x) 0))
                                  (q-checksum (q-generate-data length parsed)))))))

;; Short: 11100110111101110
;; Long:  10001101010000101

(provide 'day16)
;;; day16 ends here
