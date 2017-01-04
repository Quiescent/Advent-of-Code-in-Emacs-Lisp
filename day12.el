;;; day12 --- Solving day 12 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; cpy 1 a
;; cpy 1 b
;; cpy 26 d
;; jnz c 2
;; jnz 1 5
;; cpy 7 c
;; inc d
;; dec c
;; jnz c -2
;; cpy a c
;; inc a
;; dec b
;; jnz b -2
;; cpy c b
;; dec d
;; jnz d -6
;; cpy 13 c
;; cpy 14 d
;; inc a
;; dec d
;; jnz d -2
;; dec c
;; jnz c -5

;;; Code:

(defvar *q-registers*       nil)
(defvar *q-program*         nil)
(defvar *q-program-pointer* nil)
(defvar *q-version-2*       nil)

(defun q-parse-instruction (line)
  "Parse an instruction from LINE."
  (pcase (split-string line " " t)
    (`("cpy" ,x ,y)
     (if (string-match "[0-9]+" x)
         `(CPY-VAL ,(aref y 0) ,(string-to-number x))
       `(CPY-REG ,(aref y 0) ,(aref x 0))))
    (`("inc" ,x)    `(INC ,(aref x 0)))
    (`("dec" ,x)    `(DEC ,(aref x 0)))
    (`("jnz" ,x ,y)
     (if (string-match "[0-9]+" x)
         `(JNZ-VAL ,(string-to-number x) ,(string-to-number y))
       `(JNZ-REG ,(aref x 0) ,(string-to-number y))))))

(defun q-parse (input)
  "Parse an array of instructions from INPUT."
  (apply #'vector (mapcar #'q-parse-instruction (split-string input "\n" t " "))))

(defmacro q-run-and-advance (body)
  "Run the BODY and then increment the current program pointer."
  `(progn ,body
          (cl-incf *q-program-pointer*)))

(defun q-run-instruction ()
  "Run the current instruction and advance the program pointer by one if it wasn't a jump."
  (let ((current-instruction (aref *q-program* *q-program-pointer*)))
    (pcase current-instruction
      (`(CPY-VAL ,x ,y)
       (q-run-and-advance (puthash x y *q-registers*)))
      (`(CPY-REG ,x ,y)
       (q-run-and-advance (puthash x (gethash y *q-registers*) *q-registers*)))
      (`(INC ,x)
       (q-run-and-advance (puthash x (1+ (gethash x *q-registers*)) *q-registers*)))
      (`(DEC ,x)
       (q-run-and-advance (puthash x (1- (gethash x *q-registers*)) *q-registers*)))
      (`(JNZ-VAL ,x ,y)
       (if (/= 0 x)
           (cl-incf *q-program-pointer* y)
         (cl-incf *q-program-pointer*)))
      (`(JNZ-REG ,x ,y)
       (if (/= 0 (gethash x *q-registers*))
           (cl-incf *q-program-pointer* y)
         (cl-incf *q-program-pointer*))))))

(defun q-run-instructions (instructions)
  "Run INSTRUCTIONS on a machine we setup and produce the registers."
  (let ((*q-registers*       (make-hash-table))
        (*q-program*         instructions)
        (*q-program-pointer* 0))
    (puthash ?a 0 *q-registers*)
    (puthash ?b 0 *q-registers*)
    (puthash ?c (if *q-version-2* 1 0) *q-registers*)
    (puthash ?d 0 *q-registers*)
    (while (< *q-program-pointer* (length *q-program*))
      (q-run-instruction))
    *q-registers*))

(defun q-answer-question (input)
  "Run the instructions desrcibed by INPUT and get the value of register A."
  (interactive "sInput: ")
  (let* ((instructions  (q-parse input))
         (*q-version-2* (y-or-n-p "Set c to one first? "))
         (registers     (q-run-instructions instructions)))
    (message "%s" (gethash ?a registers))))

;; Part one: 317993
;; Part two: 9227647

(provide 'day12)
;;; day12 ends here
