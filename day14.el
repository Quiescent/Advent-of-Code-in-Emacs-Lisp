;;; day14 --- Solving day 14 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input: qzyelonm

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl))

(defvar *q-hashes-to-find*      nil)
(defvar *q-use-hash-stretching* nil)

(defconst q-times-to-hash 2016)

(defun q-first-triple (s)
  "Find the first triple of characters in the triple S."
  (let ((i   0)
        (sub "   ")
        (max (- (length s) 2))
        result)
    (while (and
            (/= max i)
            (null result))
      (aset sub 0 (aref s i))
      (aset sub 1 (aref s (1+ i)))
      (aset sub 2 (aref s (+ 2 i)))
      (when (and (= (aref sub 0) (aref sub 1))
                 (= (aref sub 1) (aref sub 2)))
        (setq result sub))
      (cl-incf i))
    result))

(defun q-create-search-string (triple)
  "Create a 5 char string by repeating the (single) char in TRIPLE."
  (let ((c   (aref triple 0))
        (res (seq-subseq "     " 0)))
    (aset res 0 c)
    (aset res 1 c)
    (aset res 2 c)
    (aset res 3 c)
    (aset res 4 c)
    res))

(defun q-hash (salt i)
  "Produce the hash of SALT and I concatenated."
  (let ((hash (secure-hash 'md5 (concat salt (number-to-string i)))))
    (if *q-use-hash-stretching*
        (dotimes (_ q-times-to-hash hash)
          (setq hash (secure-hash 'md5 hash)))
      hash)))

(defun q-search-hashes (salt)
  "Search through hashes with SALT and i to find what i was when we got the 64th valid hash."
  (let ((to-find   nil)
        (cnt       0)
        (i         0)
        (triple    nil)
        (curr-hash nil)
        (idx-hit   nil))
    (cl-labels ((count-pop-iter (xs)
                                (if (null xs)
                                    nil
                                  (pcase (car xs)
                                    (`(t . (,pos . ,_))
                                     (progn
                                       (cl-incf cnt)
                                       (when (= cnt *q-hashes-to-find*)
                                         (setq idx-hit pos))
                                       (count-pop-iter (cdr xs))))
                                    (`(nil . ,_)
                                     xs)))))
      (while (< cnt *q-hashes-to-find*)
        (setq curr-hash (q-hash salt i))
        (setq triple    (q-first-triple curr-hash))
        ;; Mark things as found (we only count them when we're removing from the front)
        (setq to-find
              (mapcar (lambda (x)
                        (pcase x
                          (`(,_ . (,pos . ,search))
                           (if (string-match search curr-hash)
                               (cons t (cons pos search))
                             x))))
                      to-find))
        (setq to-find (count-pop-iter to-find))
        (when triple
          (setq to-find (nconc to-find (list (cons nil (cons i (q-create-search-string triple)))))))
        (cl-incf i)
        (setq to-find (cl-remove-if (pcase-lambda (`(,flag . (,pos . ,_))) (and (not flag) (> (- i pos) 1000))) to-find))))
    idx-hit))

(defun q-answer-question (salt)
  "Produce the index which produced the 64th hash from SALT concatenated to an incresing integer.."
  (interactive "sSalt: ")
  (let ((*q-hashes-to-find*      64)
        (*q-use-hash-stretching* (y-or-n-p "Use stretch hashing? ")))
    (message "%s" (q-search-hashes salt))))

;; Answer part 1:          15189
;; Answer with stretching: 20864

(provide 'day14)
;;; day14 ends here
