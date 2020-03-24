(load "reversi-package")
(use-package 'reversi-package)

(reversi #'random-strategy #'random-strategy)

;; (reversi #'random-strategy #'human)

(defun mobility (player board)  ;; Peor que mix-count-mobility-3
  "The number of moves a player has."
  (length (legal-moves player board)))

;; (reversi #'human (alpha-beta-searcher 2 #'count-difference))

(defun count-difference (player board)  ;; Peor que mobility tras varios tests
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

(defun mix-count-mobility-1 (player board)  ;; Peor que mix-count-mobility-3
  (+ (count-difference player board) (mobility player board)))

(defun mix-count-mobility-2 (player board) ;; MALO
  (* (count-difference player board) (mobility player board)))

(defun mix-count-mobility-3 (player board)
  (let ((count (count-difference player board))
        (mov (mobility player board)))
    (if (<= count 0)
        (+ count mov)
        (* count mov))))

(defun mix-count-mobility-4 (player board)  ;; Peor que mix-count-mobility-3
  (let ((count (count-difference player board))
        (mov (mobility player board)))
    (if (<= count 0)
        mov
        count)))

(defparameter matrix-solution-1
  '((5 0 1 1 1 1 0 5)
    (0 1 2 2 2 2 1 0)
    (1 2 3 3 3 3 2 1)
    (1 2 3 4 4 3 2 1)
    (1 2 3 4 4 3 2 1)
    (1 2 3 3 3 3 2 1)
    (0 1 2 2 2 2 1 0)
    (5 0 1 1 1 1 0 5)))


;; Fuente http://web.eecs.utk.edu/~zzhang61/docs/reports/2014.04%20-%20Searching%20Algorithms%20in%20Playing%20Othello.pdf
(defparameter matrix-solution-2
  '((120 -20 20 5  5  20 -20 120)
    (-20 -40 -5 -5 -5 -5 -40 -20)
    (20  -5  15 3  3  15 -5  20)
    (5   -5  3  3  3  3  -5  5)
    (5   -5  3  3  3  3  -5  5)
    (20  -5  15 3  3  15 -5  20)
    (-20 -40 -5 -5 -5 -5 -40 -20)
    (120 -20 20 5  5  20 -20 120)))

(defparameter matrix-solution-3
  '((4  -3 2  2  2  2  -3 4)
    (-3 -4 -1 -1 -1 -1 -4 -3)
    (2  -1 1  0  0  1  -1 2)
    (2  -1 0  1  1  0  -1 2)
    (2  -1 0  1  1  0  -1 2)
    (2  -1 1  0  0  1  -1 2)
    (-3 -4 -1 -1 -1 -1 -4 -3)
    (4  -3 2  2  2  2  -3 4)))

(defun get-elemento (player elem-tablero elem-solucion)
  (cond ((equal player elem-tablero) elem-solucion)
        ((equal (opponent player) elem-tablero) (- 0 elem-solucion))
        (T 0)))

(defun suma-fila (player fila-tablero fila-solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (get-elemento player x y)) fila-tablero fila-solucion)))

(defun suma-matriz (player board solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (suma-fila player x y)) board solucion)))

(defun matrix-solution-1-algorith (player board)
  (suma-matriz player (get-board board) matrix-solution-1))

(defun matrix-solution-2-algorith (player board)  ;; Mejor que matrix-1
  (suma-matriz player (get-board board) matrix-solution-2))

(defun matrix-solution-3-algorith (player board)  ;; Mejor que matrix-1
  (suma-matriz player (get-board board) matrix-solution-3))

(defun mix-count-mobility-3-matrix-solution-2-algorith-1 (player board)  ;; Peor que matrix-solution-2-algorith 
  (+ (matrix-solution-2-algorith player board) (mix-count-mobility-3 player board)))

(defun mix-count-mobility-3-matrix-solution-2-algorith-2 (player board)  ;; Peor que matrix-solution-2-algorith 
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mix-count-mobility-3 player board)))
    (if (equal bar 0)
        foo
        (/ foo bar))))

(defun mobility-matrix-solution-2-algorith-1 (player board)  ;; Peor que mobility-matrix-solution-2-algorith-2
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (- foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (+ foo bar)))))

;; Para mejorar: https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun mobility-matrix-solution-2-algorith-2 (player board)  ;; VICTORIOSO MASTODONTE
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

(defun mix-count-mobility-3-matrix-solution-2-algorith-3 (player board)  ;; Peor que mobility-matrix-solution-2-algorith-2s
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mix-count-mobility-3 player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

(defun mobility-matrix-solution-3-algorith-2 (player board)  ;; VICTORIOSO MASTODONTE
  (let ((foo (matrix-solution-3-algorith player board))
        (bar (mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

;; (reversi #'random-strategy (alpha-beta-searcher 2 #'matrix-solution-1-algorith))

(round-robin
 (list (alpha-beta-searcher 2 #'mobility-matrix-solution-2-algorith-2)
       (alpha-beta-searcher 2 #'mobility-matrix-solution-3-algorith-2))
 1000
 10
 '(mobility-matrix-solution-2-algorith-2
   mobility-matrix-solution-3-algorith-2)
 )

