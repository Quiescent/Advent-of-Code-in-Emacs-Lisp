;;; day25 --- solving day 25 -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; cpy a d
;; cpy 15 c
;; cpy 170 b
;; inc d
;; dec b
;; jnz b -2
;; dec c
;; jnz c -5
;; cpy d a
;; jnz 0 0
;; cpy a b
;; cpy 0 a
;; cpy 2 c
;; jnz b 2
;; jnz 1 6
;; dec b
;; dec c
;; jnz c -4
;; inc a
;; jnz 1 -7
;; cpy 2 b
;; jnz c 2
;; jnz 1 4
;; dec b
;; dec c
;; jnz 1 -4
;; jnz 0 0
;; out b
;; jnz a -19
;; jnz 1 -21

;;; Code:

(defvar *q-registers*       nil)
(defvar *q-program*         nil)
(defvar *q-program-pointer* nil)
(defvar *q-run-with-12*     nil)
(defvar *q-output-stream*   nil)

(defconst q-outputs-to-accept 10)

(defun q-num-p (s)
  "Produce t if string S is a number."
  (string-match "[0-9]+" s))

(defun q-string-to-reg (s)
  "Produce the register index corresponding to the string S."
  (pcase s
    ("a" 0)
    ("b" 1)
    ("c" 2)
    ("d" 3)))

(defun q-parse-instruction (line)
  "Parse an instruction from LINE."
  (pcase (split-string line " " t)
    (`("cpy" ,x ,y)
     (if (q-num-p x)
         `(CPY-VAL ,(q-string-to-reg y) ,(string-to-number x))
       `(CPY-REG ,(q-string-to-reg y) ,(q-string-to-reg x))))
    (`("inc" ,x)    `(INC ,(q-string-to-reg x)))
    (`("dec" ,x)    `(DEC ,(q-string-to-reg x)))
    (`("jnz" ,x ,y)
     (if (q-num-p x)
         (if (q-num-p y)
             `(JNZ-VAL-VAL ,(string-to-number x) ,(string-to-number y))
           `(JNZ-VAL-REG ,(string-to-number x) ,(q-string-to-reg y)))
       (if (q-num-p y)
           `(JNZ-REG-VAL ,(q-string-to-reg x) ,(string-to-number y))
         `(JNZ-REG-REG ,(q-string-to-reg x) ,(q-string-to-reg y)))))
    (`("tgl" ,x)
     (if (q-num-p x)
         `(TGL-VAL ,(string-to-number x))
       `(TGL-REG ,(q-string-to-reg x))))
    (`("out" ,x)
     (if (q-num-p x)
         `(OUT-VAL ,(string-to-number x))
       `(OUT-REG ,(q-string-to-reg x))))))

(defun q-parse (input)
  "Parse an array of instructions from INPUT."
  (apply #'vector (mapcar #'q-parse-instruction (split-string input "\n" t " "))))

(defmacro q-run-and-advance (body pointer)
  "Run the BODY and then produce the incremented the current program POINTER."
  `(progn ,body
          (cl-incf ,pointer)))

(defun q-toggle-instruction (pointer)
  "Toggle the instruction at POINTER in the program."
  (when (< pointer (length *q-program*))
    (let ((current-instruction (aref *q-program* pointer)))
      (pcase current-instruction
        (`(CPY-VAL ,x ,y)
         (aset *q-program* pointer `(JNZ-VAL-REG ,y ,x)))
        (`(CPY-REG ,x ,y)
         (aset *q-program* pointer `(JNZ-REG-REG ,y ,x)))
        (`(INC ,x)
         (aset *q-program* pointer `(DEC ,x)))
        (`(DEC ,x)
         (aset *q-program* pointer `(INC ,x)))
        (`(JNZ-VAL-VAL ,x ,y)
         (aset *q-program* pointer `(CPY-VAL ,y ,x)))
        (`(JNZ-REG-VAL ,x ,y)
         (aset *q-program* pointer `(CPY-REG ,y ,x)))
        (`(JNZ-VAL-REG ,x ,y)
         (aset *q-program* pointer `(CPY-VAL ,y ,x)))
        (`(JNZ-REG-REG ,x ,y)
         (aset *q-program* pointer `(CPY-REG ,y ,x)))
        (`(TGL-REG ,x)
         (aset *q-program* pointer `(INC ,x)))))))

(defun q-run-instruction (program pointer registers)
  "Run the instruction from PROGRAM at POINTER using the given REGISTERS."
  (let ((current-instruction (aref program pointer)))
    (pcase current-instruction
      (`(CPY-VAL ,x ,y)
       (q-run-and-advance (aset registers x y) pointer))
      (`(CPY-REG ,x ,y)
       (q-run-and-advance (aset registers x (aref registers y)) pointer))
      (`(INC ,x)
       (q-run-and-advance (aset registers x (1+ (aref registers x))) pointer))
      (`(DEC ,x)
       (q-run-and-advance (aset registers x (1- (aref registers x))) pointer))
      (`(JNZ-VAL-VAL ,x ,y)
       (if (/= 0 x)
           (+ pointer y)
         (1+ pointer)))
      (`(JNZ-REG-VAL ,x ,y)
       (if (/= 0 (aref registers x))
           (+ pointer y)
         (1+ pointer)))
      (`(JNZ-VAL-REG ,x ,y)
       (if (/= 0 x)
           (+ pointer (aref registers y))
         (1+ pointer)))
      (`(JNZ-REG-REG ,x ,y)
       (if (/= 0 (aref registers x))
           (+ pointer (aref registers y))
         (1+ pointer)))
      (`(TGL-REG ,x)
       (q-run-and-advance
        (q-toggle-instruction (+ pointer (aref registers x))) pointer))
      (`(OUT-VAL ,x)
       (q-run-and-advance (setq *q-output-stream* (nconc *q-output-stream* (list x))) pointer))
      (`(OUT-REG ,x)
       (q-run-and-advance (setq *q-output-stream* (nconc *q-output-stream* (list (aref registers x)))) pointer)))))

(defun q-valid (stream)
  "Produce t if STREAM is repeating ones and zeros."
  (when (not (null stream))
    (let ((clk-val 0)
          (result  t))
      (while (and stream
                  (setq result
                        (and result
                             (= (pop stream) clk-val))))
        (setq clk-val (logxor clk-val 1)))
      result)))

(defun q-run-instructions (instructions)
  "Run INSTRUCTIONS on a machine we setup and produce the registers."
  (let ((registers       (make-vector 4 0))
        (program         instructions)
        (init            1)
        program-pointer
        *q-output-stream*)
    (while (not (q-valid *q-output-stream*))
      (setq *q-output-stream* nil)
      (setq program-pointer 0)
      (aset registers 0 init)
      (aset registers 1 0)
      (aset registers 2 0)
      (aset registers 3 0)
      (while (and (< program-pointer (length program))
                  (or (null *q-output-stream*)
                      (q-valid *q-output-stream*))
                  (< (length *q-output-stream*) q-outputs-to-accept))
        (setq program-pointer (q-run-instruction program program-pointer registers)))
      (cl-incf init))
    (1- init)))

(defun q-answer-question (input)
  "Run the instructions desrcibed by INPUT and get the value of register A."
  (interactive "sInput: ")
  (let* ((instructions    (q-parse input)))
    (message "%s" (q-run-instructions instructions))))

;; Answer: 180

(provide 'day25)
;;; day25 ends here
