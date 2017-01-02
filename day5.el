;;; day5 --- solving day 5 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; cxdnnyjw

;;; Code:

(require 'cl)
(require 'seq)

(defvar *q-current-num*        nil)
(defvar *q-chars-to-compute*   nil)
(defvar *q-hash-specifies-pos* nil)

(defun q-hex-hash-string (code)
  "Produce the MD5 hash of CODE + `*q-current-num*'."
  (md5 (concat code (number-to-string *q-current-num*))))

(defun q-add-to-result (hash result)
  "Add the correct char from HASH to RESULT.

Behaviour is toggled by `*q-hash-specifies-pos*'.  When not nil,
the position to write into is the sixth character.  Char is not
updated if it's not _."
  (if *q-hash-specifies-pos*
      (let* ((pos        (aref hash 5))
             (string-pos (string pos))
             (number-pos (string-to-number string-pos)))
        (when (and (< number-pos (length result))
                   (string-match "[0-9]" string-pos)
                   (= ?_ (aref result number-pos)))
          (aset result number-pos (aref hash 6))))
    (let ((pos 0))
      (progn
        (while (and (< pos *q-chars-to-compute*)
                    (/= ?_ (aref result pos)))
          (cl-incf pos))
        (aset result pos (aref hash 5))))))

(defun q-password (code)
  "Find the password for CODE.

Iterate finding the hash of increasing numbers concatenated to
CODE until eight hex hashes are found to start with five zeros."
  (let ((result       (make-string *q-chars-to-compute* ?_))
        (current-hash (q-hex-hash-string code)))
    (while (not (seq-every-p (lambda (x) (/= ?_ x)) result))
      (while (not (string= "00000" (substring current-hash 0 5)))
        (cl-incf *q-current-num*)
        (setq current-hash (q-hex-hash-string code)))
      (q-add-to-result current-hash result)
      (cl-incf *q-current-num*)
      (setq current-hash (q-hex-hash-string code)))
    result))

(defun q-answer-question (input)
  "Find the password from INPUT, by searching for hashes starting with five zeros in hex."
  (interactive "sCode: ")
  (let ((*q-current-num*        0)
        (*q-chars-to-compute*   8)
        (*q-hash-specifies-pos* (y-or-n-p "Hash specifies position? ")))
    (message "%s" (q-password input))))

;; Answer to simple:                        f77a0e6e
;; When the hash table specifies positions: 999828ec

(provide 'day5)
;;; day5 ends here
