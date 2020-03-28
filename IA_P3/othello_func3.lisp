;; Alias que aparece en el ranking

(defvar *alias* '|MMS3A2|)

;; Función de evaluación heurística
(defun eval-fn (player board)
  (let ((foo (matrix-solution-3-algorith player board))
          (bar (mobility player board)))
      (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
            ((and (< foo 0) (equal bar 0)) foo)
            (t (* foo bar)))))

;; Funciones auxiliares
(defparameter matrix-solution-3
  '((4  -3 2  2  2  2  -3 4)
    (-3 -4 -1 -1 -1 -1 -4 -3)
    (2  -1 1  0  0  1  -1 2)
    (2  -1 0  1  1  0  -1 2)
    (2  -1 0  1  1  0  -1 2)
    (2  -1 1  0  0  1  -1 2)
    (-3 -4 -1 -1 -1 -1 -4 -3)
    (4  -3 2  2  2  2  -3 4)))

(defun mobility (player board)  ;; Peor que mix-count-mobility-3
  "The number of moves a player has."
  (length (legal-moves player board)))

(defun get-elemento (player elem-tablero elem-solucion)
  (cond ((equal player elem-tablero) elem-solucion)
        ((equal (opponent player) elem-tablero) (- 0 elem-solucion))
        (T 0)))

(defun suma-fila (player fila-tablero fila-solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (get-elemento player x y)) fila-tablero fila-solucion)))

(defun suma-matriz (player board solucion)
  (reduce #'+ (mapcar #'(lambda (x y) (suma-fila player x y)) board solucion)))

(defun matrix-solution-3-algorith (player board)
  (suma-matriz player (get-board board) matrix-solution-3))
