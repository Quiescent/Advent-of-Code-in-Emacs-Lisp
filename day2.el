;;; day2 --- solving day two -*- lexical-binding: t; -*-

;;; Commentary:

;; Input:
;; LURLDDLDULRURDUDLRULRDLLRURDUDRLLRLRURDRULDLRLRRDDULUDULURULLURLURRRLLDURURLLUURDLLDUUDRRDLDLLRUUDURURRULURUURLDLLLUDDUUDRULLRUDURRLRLLDRRUDULLDUUUDLDLRLLRLULDLRLUDLRRULDDDURLUULRDLRULRDURDURUUUDDRRDRRUDULDUUULLLLURRDDUULDRDRLULRRRUUDUURDULDDRLDRDLLDDLRDLDULUDDLULUDRLULRRRRUUUDULULDLUDUUUUDURLUDRDLLDDRULUURDRRRDRLDLLURLULDULRUDRDDUDDLRLRRDUDDRULRULULRDDDDRDLLLRURDDDDRDRUDUDUUDRUDLDULRUULLRRLURRRRUUDRDLDUDDLUDRRURLRDDLUUDUDUUDRLUURURRURDRRRURULUUDUUDURUUURDDDURUDLRLLULRULRDURLLDDULLDULULDDDRUDDDUUDDUDDRRRURRUURRRRURUDRRDLRDUUULLRRRUDD
;; DLDUDULDLRDLUDDLLRLUUULLDURRUDLLDUDDRDRLRDDUUUURDULDULLRDRURDLULRUURRDLULUDRURDULLDRURUULLDLLUDRLUDRUDRURURUULRDLLDDDLRUDUDLUDURLDDLRRUUURDDDRLUDDDUDDLDUDDUUUUUULLRDRRUDRUDDDLLLDRDUULRLDURLLDURUDDLLURDDLULLDDDRLUDRDDLDLDLRLURRDURRRUDRRDUUDDRLLUDLDRLRDUDLDLRDRUDUUULULUDRRULUDRDRRLLDDRDDDLULURUURULLRRRRRDDRDDRRRDLRDURURRRDDULLUULRULURURDRRUDURDDUURDUURUURUULURUUDULURRDLRRUUDRLLDLDRRRULDRLLRLDUDULRRLDUDDUUURDUDLDDDUDL
;; RURDRUDUUUUULLLUULDULLLDRUULURLDULULRDDLRLLRURULLLLLLRULLURRDLULLUULRRDURRURLUDLULDLRRULRDLDULLDDRRDLLRURRDULULDRRDDULDURRRUUURUDDURULUUDURUULUDLUURRLDLRDDUUUUURULDRDUDDULULRDRUUURRRDRLURRLUUULRUDRRLUDRDLDUDDRDRRUULLLLDUUUULDULRRRLLRLRLRULDLRURRLRLDLRRDRDRLDRUDDDUUDRLLUUURLRLULURLDRRULRULUDRUUURRUDLDDRRDDURUUULLDDLLDDRUDDDUULUDRDDLULDDDDRULDDDDUUUURRLDUURULRDDRDLLLRRDDURUDRRLDUDULRULDDLDDLDUUUULDLLULUUDDULUUDLRDRUDLURDULUDDRDRDRDDURDLURLULRUURDUDULDDLDDRUULLRDRLRRUURRDDRDUDDLRRLLDRDLUUDRRDDDUUUDLRRLDDDUDRURRDDUULUDLLLRUDDRULRLLLRDLUDUUUUURLRRUDUDDDDLRLLULLUDRDURDDULULRDRDLUDDRLURRLRRULRL
;; LDUURLLULRUURRDLDRUULRDRDDDRULDLURDDRURULLRUURRLRRLDRURRDRLUDRUUUULLDRLURDRLRUDDRDDDUURRDRRURULLLDRDRDLDUURLDRUULLDRDDRRDRDUUDLURUDDLLUUDDULDDULRDDUUDDDLRLLLULLDLUDRRLDUUDRUUDUDUURULDRRLRRDLRLURDRURURRDURDURRUDLRURURUUDURURUDRURULLLLLUDRUDUDULRLLLRDRLLRLRLRRDULRUUULURLRRLDRRRDRULRUDUURRRRULDDLRULDRRRDLDRLUDLLUDDRURLURURRLRUDLRLLRDLLDRDDLDUDRDLDDRULDDULUDDLLDURDULLDURRURRULLDRLUURURLLUDDRLRRUUDULRRLLRUDRDUURLDDLLURRDLRUURLLDRDLRUULUDURRDULUULDDLUUUDDLRRDRDUDLRUULDDDLDDRUDDD
;; DRRDRRURURUDDDRULRUDLDLDULRLDURURUUURURLURURDDDDRULUDLDDRDDUDULRUUULRDUDULURLRULRDDLDUDLDLULRULDRRLUDLLLLURUDUDLLDLDRLRUUULRDDLUURDRRDLUDUDRULRRDDRRLDUDLLDLURLRDLRUUDLDULURDDUUDDLRDLUURLDLRLRDLLRUDRDUURDDLDDLURRDDRDRURULURRLRLDURLRRUUUDDUUDRDRULRDLURLDDDRURUDRULDURUUUUDULURUDDDDUURULULDRURRDRDURUUURURLLDRDLDLRDDULDRLLDUDUDDLRLLRLRUUDLUDDULRLDLLRLUUDLLLUUDULRDULDLRRLDDDDUDDRRRDDRDDUDRLLLDLLDLLRDLDRDLUDRRRLDDRLUDLRLDRUURUDURDLRDDULRLDUUUDRLLDRLDLLDLDRRRLLULLUDDDLRUDULDDDLDRRLLRDDLDUULRDLRRLRLLRUUULLRDUDLRURRRUULLULLLRRURLRDULLLRLDUUUDDRLRLUURRLUUUDURLRDURRDUDDUDDRDDRUD

;;; Code:

(defvar *q-position*    nil)
(defvar *q-block-count* nil)
(defvar *q-x-dim*       nil)
(defvar *q-board*       nil)

(defconst q-simple-board
  [["1" "2" "3"]
   ["4" "5" "6"]
   ["7" "8" "9"]])

(defconst q-advanced-board
  [["_" "_" "1" "_" "_"]
   ["_" "2" "3" "4" "_"]
   ["5" "6" "7" "8" "9"]
   ["_" "A" "B" "C" "_"]
   ["_" "_" "D" "_" "_"]])

(defmacro q-when-in-bounds (new-position body)
  "If NEW-POSITION is in bounds Execute BODY."
  `(when (and (>= ,new-position 0)
              (<  ,new-position *q-block-count*)
              (not (equal "_" (q-index-board new-position))))
     ,body))

(defun q-index-board (position)
  "Produce the value in the board at POSITION."
  (aref (aref *q-board* (/ position *q-x-dim*))
        (mod position *q-x-dim*)))

(defun q-move-up ()
  "Move up from the current positon."
  (let ((new-position (- *q-position* *q-x-dim*)))
    (q-when-in-bounds new-position
                      (setq *q-position* new-position))))

(defun q-row-unchanged-p (new-position)
  "Produce the difference in rows between NEW-POSITION and the current position."
  (= (/ *q-position* *q-x-dim*) (/ new-position *q-x-dim*)))

(defun q-move-left ()
  "Move left from the current position."
  (let* ((new-position (1- *q-position*)))
    (when (q-row-unchanged-p new-position)
      (q-when-in-bounds new-position
                        (setq *q-position*
                              new-position)))))

(defun q-move-down ()
  "Move down from the current position."
  (let ((new-position (+ *q-position* *q-x-dim*)))
    (q-when-in-bounds new-position
                      (setq *q-position* new-position))))

(defun q-move-right ()
  "Move right from the current position."
  (let* ((new-position (1+ *q-position*)))
    (when (q-row-unchanged-p new-position)
      (q-when-in-bounds new-position
                        (setq *q-position*
                              new-position)))))

(defun q-parse-input (input)
  "Parse INPUT into a list of lists of directions to move."
  (cl-labels ((parse-move (x) (cond
                               ((string= x "L") 'L)
                               ((string= x "R") 'R)
                               ((string= x "U") 'U)
                               ((string= x "D") 'D))))
    (mapcar (lambda (xs)
              (mapcar (lambda (x)
                        (parse-move x))
                      xs))
            (mapcar (lambda (line)
                      (split-string line "" t))
                    (split-string input "\n")))))

(defun q-go (instructions)
  "Follow INSTRUCTIONS to produce a code by hopping about on a keypad."
  (cl-labels ((move (x) (cond
                         ((eq x 'L) (q-move-left))
                         ((eq x 'R) (q-move-right))
                         ((eq x 'U) (q-move-up))
                         ((eq x 'D) (q-move-down)))))
    (mapcar (lambda (moves)
              (progn (mapc (lambda (move) (move move))
                           moves)
                     (q-index-board *q-position*)))
            instructions)))

(defun q-answer-question (input)
  "Find the correct code as per the description: INPUT."
  (interactive "sInput: ")
  (let* ((instructions    (q-parse-input input))
         (simple-board    (y-or-n-p "Use simple board? "))
         (*q-board*       (if simple-board q-simple-board q-advanced-board))
         (*q-position*    (if simple-board 4 10))
         (*q-block-count* (apply #'+ (seq-map #'seq-length *q-board*)))
         (*q-x-dim*       (seq-length (seq-elt *q-board* 0))))
    (message "%s" (mapconcat #'identity (q-go instructions) ""))))

;; simple   board: 97289
;; advanced board: 9A7DC

(provide 'day2)
;;; day2 ends here
