;;; day23 --- Solving day 23 -*- lexical-binding: t; -*-

;;; Commentary:

;; cpy a b
;; dec b
;; cpy a d
;; cpy 0 a
;; cpy b c
;; inc a
;; dec c
;; jnz c -2
;; dec d
;; jnz d -5
;; dec b
;; cpy b c
;; cpy c d
;; dec d
;; inc c
;; jnz d -2
;; tgl c
;; cpy -16 c
;; jnz 1 c
;; cpy 81 c
;; jnz 93 d
;; inc a
;; inc d
;; jnz d -2
;; inc c
;; jnz c -5

;;; Code:

(defvar *q-registers*       nil)
(defvar *q-program*         nil)
(defvar *q-program-pointer* nil)
(defvar *q-run-with-12*     nil)

(defun q-num-p (s)
  "Produce t if string S is a number."
  (string-match "[0-9]+" s))

(defun q-parse-instruction (line)
  "Parse an instruction from LINE."
  (pcase (split-string line " " t)
    (`("cpy" ,x ,y)
     (if (q-num-p x)
         `(CPY-VAL ,(aref y 0) ,(string-to-number x))
       `(CPY-REG ,(aref y 0) ,(aref x 0))))
    (`("inc" ,x)    `(INC ,(aref x 0)))
    (`("dec" ,x)    `(DEC ,(aref x 0)))
    (`("jnz" ,x ,y)
     (if (q-num-p x)
         (if (q-num-p y)
             `(JNZ-VAL-VAL ,(string-to-number x) ,(string-to-number y))
           `(JNZ-VAL-REG ,(string-to-number x) ,(aref y 0)))
       (if (q-num-p y)
           `(JNZ-REG-VAL ,(aref x 0) ,(string-to-number y))
         `(JNZ-REG-REG ,(aref x 0) ,(aref y 0)))))
    (`("tgl" ,x)
     (if (q-num-p x)
         `(TGL-VAL ,(string-to-number x))
       `(TGL-REG ,(aref x 0))))))

(defun q-parse (input)
  "Parse an array of instructions from INPUT."
  (apply #'vector (mapcar #'q-parse-instruction (split-string input "\n" t " "))))

(defmacro q-run-and-advance (body)
  "Run the BODY and then increment the current program pointer."
  `(progn ,body
          (cl-incf *q-program-pointer*)))

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
      (`(JNZ-VAL-VAL ,x ,y)
       (if (/= 0 x)
           (cl-incf *q-program-pointer* y)
         (cl-incf *q-program-pointer*)))
      (`(JNZ-REG-VAL ,x ,y)
       (if (/= 0 (gethash x *q-registers*))
           (cl-incf *q-program-pointer* y)
         (cl-incf *q-program-pointer*)))
      (`(JNZ-VAL-REG ,x ,y)
       (if (/= 0 x)
           (cl-incf *q-program-pointer* (gethash y *q-registers*))
         (cl-incf *q-program-pointer*)))
      (`(JNZ-REG-REG ,x ,y)
       (if (/= 0 (gethash x *q-registers*))
           (cl-incf *q-program-pointer* (gethash y *q-registers*))
         (cl-incf *q-program-pointer*)))
      (`(TGL-REG ,x)
       (q-run-and-advance
        (q-toggle-instruction (+ *q-program-pointer* (gethash x *q-registers*))))))))

(defun q-run-instructions (instructions)
  "Run INSTRUCTIONS on a machine we setup and produce the registers."
  (let ((*q-registers*       (make-hash-table))
        (*q-program*         instructions)
        (*q-program-pointer* 0))
    (puthash ?a (if *q-run-with-12* 12 7) *q-registers*)
    (puthash ?b 0 *q-registers*)
    (puthash ?c 0 *q-registers*)
    (puthash ?d 0 *q-registers*)
    (while (< *q-program-pointer* (length *q-program*))
      (q-run-instruction))
    *q-registers*))

(defun q-answer-question (input)
  "Run the instructions desrcibed by INPUT and get the value of register A."
  (interactive "sInput: ")
  (let* ((instructions    (q-parse input))
         (*q-run-with-12* (y-or-n-p "Run with 12 eggs? "))
         (registers       (q-run-instructions instructions)))
    (message "%s" (gethash ?a registers))))

;; Simple answer: 12573
;; With 6:        8253
;; With 7:        12573
;; With 8:        47853
;; With 9:        370413
;; With 10:       3636333
;; With 11:       39924333
;; With 12:       479009133
;; There's a pattern here, but I'm not finding it.

(provide 'day23)
;;; day23 ends here
