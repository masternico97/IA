;; Alias que aparece en el ranking

(defvar *alias* '|BMMS2A2|)

;; Función de evaluación heurística
(defun eval-fn (player board)
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board)))
    (+  (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
              ((and (< foo 0) (equal bar 0)) foo)
              (t (* foo bar)))
        (other-mobility player board))))

;; Funciones auxiliares
(defparameter matrix-solution-2
  '((120 -20 20 5  5  20 -20 120)
    (-20 -40 -5 -5 -5 -5 -40 -20)
    (20  -5  15 3  3  15 -5  20)
    (5   -5  3  3  3  3  -5  5)
    (5   -5  3  3  3  3  -5  5)
    (20  -5  15 3  3  15 -5  20)
    (-20 -40 -5 -5 -5 -5 -40 -20)
    (120 -20 20 5  5  20 -20 120)))

(defun mobility (player board)  ;; Peor que mix-count-mobility-3
  "The number of moves a player has."
  (length (legal-moves player board)))

(defun other-mobility (player board)
  (- 0 (length (legal-moves (opponent player) board))))

(defun get-elemento (player elem-tablero elem-solucion)
  (cond ((equal player elem-tablero) elem-solucion)
        ((equal (opponent player) elem-tablero) (- 0 elem-solucion))
        (T 0)))

(defun suma-fila (player fila-tablero fila-solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (get-elemento player x y)) fila-tablero fila-solucion)))

(defun suma-matriz (player board solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (suma-fila player x y)) board solucion)))

(defun matrix-solution-2-algorith (player board)
  (suma-matriz player (get-board board) matrix-solution-2))
