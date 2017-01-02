;;; day21 --- solving day 21 -*- lexical-binding: t; -*-

;;; Commentary:

;; Possible instructions:
;;  - swap position X with position Y
;;  - swap letter X with letter Y
;;  - rotate left/right X steps
;;  - rotate based on position of letter X
;;  - reverse positions X through Y
;;  - move position X to position Y

;; Test input.  Code: abcde.
;; swap position 4 with position 0
;; swap letter d with letter b
;; reverse positions 0 through 4
;; rotate left 1 step
;; move position 1 to position 4
;; move position 3 to position 0
;; rotate based on position of letter b
;; rotate based on position of letter d

;; Input:
;; Scramble)   abcdefgh
;; Unscramble) fbgdceah

;;; Code:

(defun q-parse-line (line)
  "Parse LINE into an instruction."
  (pcase (split-string line " " t)
    (`("swap" "position" ,x "with" "position" ,y)
     `(SWAP-POS ,(string-to-number x) ,(string-to-number y)))
    (`("swap" "letter" ,x "with" "letter" ,y)
     `(SWAP-LET ,(aref x 0) ,(aref y 0)))
    (`("rotate" "right" ,x "steps")
     `(ROT ,(- 0 (string-to-number x))))
    (`("rotate" "right" ,x "step")
     `(ROT ,(- 0 (string-to-number x))))
    (`("rotate" "left" ,x "steps")
     `(ROT ,(string-to-number x)))
    (`("rotate" "left" ,x "step")
     `(ROT ,(string-to-number x)))
    (`("rotate" "based" "on" "position" "of" "letter" ,x)
     `(ROT-POS  ,(aref x 0)))
    (`("reverse" "positions" ,x "through" ,y)
     `(REV-POS ,(string-to-number x) ,(string-to-number y)))
    (`("move" "position" ,x "to" "position" ,y)
     `(MOV-POS ,(string-to-number x) ,(string-to-number y)))
    (_ (error "Couldn't parse instruction: \"%s\"" line))))

(defun q-parse (input)
  "Pares INPUT into an input string and a series of instructions."
  (let ((lines (split-string input "\n" t " ")))
    (mapcar #'q-parse-line lines)))

(defun q-find-index (char string)
  "Find the index of CHAR in STRING."
  (let ((idx 0)
        found)
    (while (null found)
      (if (= char (aref string idx))
          (setq found t)
        (cl-incf idx)))
    idx))

(defun q-evaluate-inst (instruction code)
  "Evaluate INSTRUCTION on CODE."
  (pcase instruction
    (`(SWAP-POS ,x ,y)
     (let ((old-x (aref code x)))
       (aset code x (aref code y))
       (aset code y old-x)))
    (`(SWAP-LET ,x ,y)
     (dotimes (i (length code))
       (if (= x (aref code i))
           (aset code i y)
         (when (= y (aref code i))
           (aset code i x)))))
    (`(ROT ,x)
     (let ((orig-code (seq-subseq code 0))
           (length    (length code)))
       (dotimes (i length)
         (aset code i (aref orig-code (mod (+ i x) length))))))
    (`(ROT-POS  ,x)
     (let* ((orig-code (seq-subseq code 0))
            (length    (length code))
            (rot       (q-find-index x code))
            (adjusted  (- 0 (+ rot 1 (if (>= rot 4) 1 0)))))
       (dotimes (i length)
         (aset code i (aref orig-code (mod (+ i adjusted) length))))))
    ;; Assume that we're not wrapping
    (`(REV-POS ,x ,y)
     (let ((orig-code (seq-subseq code 0))
           (length    (- y x)))
       (dotimes (i (1+ length))
         (aset code (+ i x) (aref orig-code (+ x (- length i)))))))
    (`(MOV-POS ,x ,y)
     (let* ((minus-x (concat (substring code 0 x) (substring code (1+ x))))
            (new-code (concat (substring minus-x 0 y) (string (aref code x)) (substring minus-x y))))
       (dotimes (i (length code))
         (aset code i (aref new-code i)))))
    (_ (error "Unknown instruction \"%s\"" instruction))))

(defun q-evaluate (instructions code)
  "Evaluate INSTRUCTIONS on CODE."
  (mapc (lambda (inst) (q-evaluate-inst inst code)) instructions))

(defun q-permutations (code)
  "Produce the permutations of CODE."
  (mapcar #'concat (-permutations (seq-map #'identity code))))

(defun q-find-input-for (output instructions)
  "Find the input code which generated OUTPUT from INSTRUCTIONS."
  (cl-find-if (lambda (code) (progn
                               (let ((scratch-code (seq-subseq code 0)))
                                 (q-evaluate instructions scratch-code)
                                 (string= scratch-code output))))
              (q-permutations output)))

(defun q-answer-question (input code)
  "Evaluate the program defined by INPUT on CODE."
  (interactive "sInput: \nsCode: ")
  (let ((instructions (q-parse input)))
    (if (y-or-n-p "Reverse the code? ")
        (message "%s" (q-find-input-for code instructions))
      (progn (q-evaluate instructions code)
             (message code)))))

;; Execute: bdfhgeca
;; Reverse: gdfcabeh

(provide 'day21)
;;; day21 ends here
