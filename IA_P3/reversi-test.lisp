(load "reversi-package")
(use-package 'reversi-package)

;; (reversi #'random-strategy #'random-strategy)

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

(defun other-mobility (player board)
  (- 0 (length (legal-moves (opponent player) board))))

(defun both-mobility (player board)
  (+ (mobility player board) (other-mobility player board)))

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

(defun mix-both-count-mobility-3 (player board)
  (let ((count (count-difference player board))
        (mov (both-mobility player board)))
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

(defun other-mobility-matrix-solution-2-algorith-1 (player board)  ;; Peor que mobility-matrix-solution-2-algorith-2
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (other-mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

(defun both-mobility-matrix-solution-2-algorith-1 (player board)  ;; Peor que mobility-matrix-solution-2-algorith-2
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (both-mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

(defun both-mobility-matrix-solution-2-algorith-2 (player board)  ;; Mejor que mobility-matrix-solution-2-algorith-2 local, peor torneo
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board)))
    (+  (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
              ((and (< foo 0) (equal bar 0)) foo)
              (t (* foo bar)))
        (other-mobility player board))))

(defun both-mobility-matrix-solution-3-algorith-2 (player board)  ;; Peor que mobility-matrix-solution-3-algorith-2
  (let ((foo (matrix-solution-3-algorith player board))
        (bar (mobility player board)))
    (+  (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
              ((and (< foo 0) (equal bar 0)) foo)
              (t (* foo bar)))
        (other-mobility player board))))

(defun count-difference-both-mobility-matrix-solution-2-algorith-1 (player board)  ;; Peor que both-mobility-matrix-solution-2-algorith-2
  (let ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board)))
    (+  (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
              ((and (< foo 0) (equal bar 0)) foo)
              (t (* foo bar)))
        (other-mobility player board)
        (count-difference player board))))

(defun mix-3-count-difference-both-mobility-matrix-solution-2-algorith-1 (player board) ;; Peor que count-difference-both-mobility-matrix-solution-2-algorith-1
  (let* ((foo (matrix-solution-2-algorith player board))
        (bar (mobility player board))
        (aux (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
              ((and (< foo 0) (equal bar 0)) foo)
              (t (* foo bar))))
        (count (count-difference player board)))
    (if (<= count 0)
        (+ count (other-mobility player board) aux)
        (* count (+ (other-mobility player board) aux)))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defparameter *weights*
  '#(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun weighted-squares (player board)
  "Sum of the weights of player's squares minus opponent's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
          when (eql (bref board i) player)
          sum (aref *weights* i)
          when (eql (bref board i) opp)
          sum (- (aref *weights* i)))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun modified-weighted-squares (player board)
  "Like WEIGHTED-SQUARES, but don't take off for moving
  near an occupied corner."
  (let ((w (weighted-squares player board)))
    (dolist (corner '(11 18 81 88))
      (when (not (eql (bref board corner) empty))
        (dolist (c (neighbors corner))
          (when (not (eql (bref board c) empty))
            (incf w (* (- 5 (aref *weights* c))
                       (if (eql (bref board c) player)
                           +1 -1)))))))
    w))

(defun mobility-modified-weighted-squares (player board)  ;; ESTA ES GOOD
  (let ((foo (modified-weighted-squares player board))
        (bar (mobility player board)))
    (cond ((and (< foo 0) (not (equal bar 0))) (/ foo bar))
          ((and (< foo 0) (equal bar 0)) foo)
          (t (* foo bar)))))

(defun both-mobility-modified-weighted-squares(player board)
    (+  (mobility-modified-weighted-squares player board)
        (other-mobility player board)))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun new-mobility (player board)  ;; Peor que both mobility
  "Current Mobility is the number of legal moves.
  Potential mobility is the number of blank squares
  adjacent to an opponent that are not legal moves.
  Returns current and potential mobility for player."
  (let ((opp (opponent player))
        (current 0)    ; player's current mobility
        (potential 0)) ; player's potential mobility
    (dolist (square all-squares)
      (when (eql (bref board square) empty)
        (cond ((legal-p square player board)
               (incf current))
              ((some #'(lambda (sq) (eql (bref board sq) opp))
                     (neighbors square))
               (incf potential)))))
    (values current (+ current potential))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defvar *edge-table* (make-array (expt 3 10))
  "Array of values to player-to-move for edge positions.")

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defconstant edge-and-x-lists
  '((22 11 12 13 14 15 16 17 18 27)
    (72 81 82 83 84 85 86 87 88 77)
    (22 11 21 31 41 51 61 71 81 72)
    (27 18 28 38 48 58 68 78 88 77))
  "The four edges (with their X-squares).")

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun edge-index (player board squares)
  "The index counts 1 for player; 2 for opponent,
  on each square---summed as a base 3 number."
  (let ((index 0))
    (dolist (sq squares)
      (setq index (+ (* index 3)
                     (cond ((eql (bref board sq) empty) 0)
                           ((eql (bref board sq) player) 1)
                           (t 2)))))
    index))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun edge-stability (player board)
  "Total edge evaluation for player to move on board."
  (loop for edge-list in edge-and-x-lists
        sum (aref *edge-table*
                  (edge-index player board edge-list))))  

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defconstant top-edge (first edge-and-x-lists))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun init-edge-table ()
  "Initialize *edge-table*, starting from the empty board."
  ;; Initialize the static values
  (loop for n-pieces from 0 to 10 do
        (map-edge-n-pieces
          #'(lambda (board index)
              (setf (aref *edge-table* index)
                    (static-edge-stability black board)))
          black (initial-board) n-pieces top-edge 0))
  ;; Now iterate five times trying to improve:
  (dotimes (i 5)
    ;; Do the indexes with most pieces first
    (loop for n-pieces from 9 downto 1 do
          (map-edge-n-pieces
            #'(lambda (board index)
                (setf (aref *edge-table* index)
                      (possible-edge-moves-value
                        black board index)))
            black (initial-board) n-pieces top-edge 0))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun map-edge-n-pieces (fn player board n squares index)
  "Call fn on all edges with n pieces."
  ;; Index counts 1 for player; 2 for opponent
  (cond
    ((< (length squares) n) nil)
    ((null squares) (funcall fn board index))
    (t (let ((index3 (* 3 index))
             (sq (first squares)))
         (map-edge-n-pieces fn player board n (rest squares) index3)
         (when (and (> n 0) (eql (bref board sq) empty))
           (setf (bref board sq) player)
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 1 index3))
           (setf (bref board sq) (opponent player))
           (map-edge-n-pieces fn player board (- n 1) (rest squares)
                              (+ 2 index3))
           (setf (bref board sq) empty))))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun possible-edge-moves-value (player board index)
  "Consider all possible edge moves.
  Combine their values into a single number."
  (combine-edge-moves
    (cons
      (list 1.0 (aref *edge-table* index)) ;; no move
      (loop for sq in top-edge             ;; possible moves
            when (eql (bref board sq) empty)
            collect (possible-edge-move player board sq)))
    player))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defvar *ply-boards*
  (apply #'vector (loop repeat 40 collect (initial-board))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun possible-edge-move (player board sq)
  "Return a (prob val) pair for a possible edge move."
  (let ((new-board (replace (aref *ply-boards* player) board)))
    (make-move sq player new-board)
    (list (edge-move-probability player board sq)
          (- (aref *edge-table*
                   (edge-index (opponent player)
                               new-board top-edge))))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun combine-edge-moves (possibilities player)
  "Combine the best moves."
  (let ((prob 1.0)
        (val 0.0)
        (fn (if (eql player black) #'> #'<)))
    (loop for pair in (sort possibilities fn :key #'second)
          while (>= prob 0.0)
          do (incf val (* prob (first pair) (second pair)))
             (decf prob (* prob (first pair))))
    (round val)))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(let ((corner/xsqs '((11 . 22) (18 . 27) (81. 72) (88 . 77))))
  (defun corner-p (sq) (assoc sq corner/xsqs))
  (defun x-square-p (sq) (rassoc sq corner/xsqs))
  (defun x-square-for (corner) (cdr (assoc corner corner/xsqs)))
  (defun corner-for (xsq) (car (rassoc xsq corner/xsqs))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun edge-move-probability (player board square)
  "What's the probability that player can move to this square?"
  (cond
    ((x-square-p square) .5) ;; X-squares
    ((legal-p square player board) 1.0) ;; immediate capture
    ((corner-p square) ;; move to corner depends on X-square
     (let ((x-sq (x-square-for square)))
       (cond
         ((eql (bref board x-sq) empty) .1)
         ((eql (bref board x-sq) player) 0.001)
         (t .9))))
    (t (/ (aref
            '#2A((.1  .4 .7)
                 (.05 .3  *)
                 (.01  *  *))
            (count-edge-neighbors player board square)
            (count-edge-neighbors (opponent player) board square))
          (if (legal-p square (opponent player) board) 2 1)))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun count-edge-neighbors (player board square)
  "Count the neighbors of this square occupied by player."
  (count-if #'(lambda (inc)
                (eql (bref board (+ square inc)) player))
            '(+1 -1)))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defparameter *static-edge-table*
  '#2A(;stab  semi    un
       (   *    0 -2000) ; X
       ( 700    *     *) ; corner
       (1200  200   -25) ; C
       (1000  200    75) ; A
       (1000  200    50) ; B
       (1000  200    50) ; B
       (1000  200    75) ; A
       (1200  200   -25) ; C
       ( 700    *     *) ; corner
       (   *    0 -2000) ; X
       ))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(defun static-edge-stability (player board)
  "Compute this edge's static stability"
  (loop for sq in top-edge
        for i from 0
        sum (cond
              ((eql (bref board sq) empty) 0)
              ((eql (bref board sq) player)
               (aref *static-edge-table* i
                     (piece-stability board sq)))
              (t (- (aref *static-edge-table* i
                          (piece-stability board sq)))))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md
(let ((stable 0) (semi-stable 1) (unstable 2))

  (defun piece-stability (board sq)
    (cond
      ((corner-p sq) stable)
      ((x-square-p sq)
       (if (eql (bref board (corner-for sq)) empty)
           unstable semi-stable))
      (t (let* ((player (bref board sq))
                (opp (opponent player))
                (p1 (find player board :test-not #'eql
                          :start sq :end 19))
                (p2 (find player board :test-not #'eql
                          :start 11 :end sq
                          :from-end t)))
           (cond
             ;; unstable pieces can be captured immediately
             ;; by playing in the empty square
             ((or (and (eql p1 empty) (eql p2 opp))
                  (and (eql p2 empty) (eql p1 opp)))
              unstable)
             ;; Semi-stable pieces might be captured
             ((and (eql p1 opp) (eql p2 opp)
                   (find empty board :start 11 :end 19))
              semi-stable)
             ((and (eql p1 empty) (eql p2 empty))
              semi-stable)
             ;; Stable pieces can never be captured
             (t stable)))))))

;; Sustitucion de *move-number*
(defun calc-move-number (player board)
  "Count player's pieces plus opponent's pieces - 4."
  (let ((brd (get-board board)))
    (+ (- 0 4)
       (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

;; Copiado de https://github.com/norvig/paip-lisp/blob/master/docs/chapter18.md (Algo debo estar haciendo mal)
(defun Iago-eval (player board)
  "Combine edge-stability, current mobility and
  potential mobility to arrive at an evaluation."
  ;; The three factors are multiplied by coefficients
  ;; that vary by move number:
  (let* ((move-number (calc-move-number player board))
         (c-edg (+ 312000 (* 6240 move-number)))
         (c-cur (if (< move-number 25)
                    (+ 50000 (* 2000 move-number))
                    (+ 75000 (* 1000 move-number))))
         (c-pot 20000))
    (multiple-value-bind (p-cur p-pot)
        (new-mobility player board)
      (multiple-value-bind (o-cur o-pot)
          (new-mobility (opponent player) board)
        ;; Combine the three factors into one sum:
        (+ (round (* c-edg (edge-stability player board)) 32000)
           (round (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2))
           (round (* c-pot  (- p-pot o-pot)) (+ p-pot o-pot 2)))))))
;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

;; (reversi #'random-strategy (alpha-beta-searcher 2 #'matrix-solution-1-algorith))

(round-robin
 (list (alpha-beta-searcher 2 #'mobility-modified-weighted-squares)
       (alpha-beta-searcher 2 #'Iago-eval)
       )
 200
 10
 '(mobility-modified-weighted-squares
   Iago-eval
   )
 )
