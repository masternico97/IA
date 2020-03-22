(defpackage :reversi-package
  (:use :common-lisp)
  (:export :empty
	   :black 
	   :white
	   :outer
	   :get-board
	   :all-squares
	   :opponent
	   :name-of
	   :bref
	   :copy-board 
	   :initial-board
	   :valid-p 
	   :legal-p 
	   :make-move 
	   :make-flips
	   :would-flip?
	   :find-bracketing-piece
	   :next-to-play 
	   :any-legal-move? 
	   :random-strategy 
	   :legal-moves 
	   :maximizer 
	   :final-value 
	   :minimax 
	   :minimax-searcher
	   :alpha-beta 
	   :alpha-beta-searcher 
	   :neighbors 
	   :h8->88 
	   :88->h8 
	   :human 
	   :reversi 
	   :get-move 
	   :get-board 
	   :print-board 
	   :time-string 
	   :random-reversi-series 
	   :switch-strategies 
	   :reversi-series 
	   :round-robin))

(in-package reversi-package)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;(defvar *my-random-state* (make-random-state t))

;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something
;;; else.  This has not been done (for compatibility with the book).

(defun mysymbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; One bug fix by Alberto Segre, segre@cs.cornell.edu, March 1993.

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defconstant all-directions '(-11 -10 -9 -1 1 9 10 11))

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,empty ,outer))

(defun name-of (piece) (char ".@O?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defconstant all-squares
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER.  Initially
  ;; the 4 center squares are taken, the others empty.
  (let ((board (make-array 100 :element-type 'piece
                           :initial-element outer)))
    (dolist (square all-squares)
      (setf (bref board square) empty))
    (setf (bref board 44) white   (bref board 45) black
          (bref board 54) black   (bref board 55) white)
    board))

(defun valid-p (move)
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some #'(lambda (dir) (would-flip? move player board dir))
             all-directions)))

(defun make-move (move player board)
  "Update board to reflect move by player"
  ;; First make the move, then make any flips
  (setf (bref board move) player)
  (dolist (dir all-directions)
    (make-flips move player board dir))
  board)

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (let ((c (+ move dir)))
	(loop while (not (eql c bracketer)) do
	      (setf (bref board c) player)
	      (setf c (+ c dir))))))
	;(loop for c from (+ move dir) by dir until (eql c bracketer)
        ;      do (setf (bref board c) player)))))
  )

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by
  ;; one of player's pieces
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
        ((eql (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (t nil)))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board)
           (when print
             (format t "~&~c has no moves and must pass."
                     (name-of opp)))
           previous-player)
          (t nil))))

(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (some #'(lambda (move) (legal-p move player board))
        all-squares))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (loop for move in all-squares
	when (legal-p move player board) collect move))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board"
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
             (scores (mapcar #'(lambda (move)
				 (funcall
				  eval-fn
				  player
				  (make-move move player
					     (copy-board board))))
                             moves))
             (best  (apply #'max scores)))
        (elt moves (position best scores)))))

(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(defun final-value (player board)
  "Is this a win, loss, or draw for player?"
  (case (signum (count-difference player board))
    (-1 losing-value)
    ( 0 0)
    (+1 winning-value)))

(defun minimax (player board ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (minimax (opponent player) board
                            (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (minimax
                                 (opponent player) board2
                                 (- ply 1) eval-fn))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (minimax player board ply eval-fn)
        (declare (ignore value))
        move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent player) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move (first moves)))
              (loop for move in moves do
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (alpha-beta
                                 (opponent player) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) eval-fn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board losing-value winning-value
                      depth eval-fn)
        (declare (ignore value))
        move)))

(let ((neighbor-table (make-array 100 :initial-element nil)))
  ;; Initialize the neighbor table
  (dolist (square all-squares)
    (dolist (dir all-directions)
      (if (valid-p (+ square dir))
          (push (+ square dir)
                (aref neighbor-table square)))))

  (defun neighbors (square)
    "Return a list of all squares adjacent to a square."
    (aref neighbor-table square)))

(let ((square-names
        (cross-product #'mysymbol
                       '(? a b c d e f g h ?)
                       '(? 1 2 3 4 5 6 7 8 ?))))

  (defun h8->88 (str)
    "Convert from alphanumeric to numeric square notation."
    (or (position (string str) square-names :test #'string-equal)
        str))

  (defun 88->h8 (num)
    "Convert from numeric to alphanumeric square notation."
    (if (valid-p num)
        (elt square-names num)
        num)))

(defun human (player board)
  "A human player for the game of Reversi"
  (let ((legal (legal-moves player board)))
    (loop
     (format t "~&~c to move ~a:~%" (name-of player) (mapcar #'88->h8 legal))
     (let ((move (h8->88 (read))))
       (if (member move legal)
	   (return move))))))

(defvar *move-number* 1 "The number of the move to be played")

(defun reversi (bl-strategy wh-strategy
                &optional (print t) (minutes 30))
  "Play a game of reversi.  Return the score, where a positive
  difference means black, the first player, wins."
  (let ((board (initial-board))
        (clock (make-array (+ 1 (max black white))
                           :initial-element
                           (* minutes 60
                              internal-time-units-per-second))))
    (catch 'game-over
      (loop for *move-number* from 1
            for player = black then (next-to-play board player print)
            for strategy = (if (eql player black)
                               bl-strategy
                               wh-strategy)
            until (null player)
            do (get-move strategy player board print clock))
      (when print
        (format t "~&The game is over.  Final result:")
        (print-board board clock))
      (count-difference black board))))

(defvar *clock* (make-array 3) "A copy of the game clock")
(defvar *board* (initial-board) "A copy of the game board")

(defun get-move (strategy player board print clock)
  "Call the player's strategy function to get a move.
  Keep calling until a legal move is made."
  ;; Note we don't pass the strategy function the REAL board.
  ;; If we did, it could cheat by changing the pieces on the board.
  (when print (print-board board clock))
  (replace *clock* clock)
  (let* ((t0 (get-internal-real-time))
         (move (funcall strategy player (replace *board* board)))
         (t1 (get-internal-real-time)))
    (decf (elt clock player) (- t1 t0))
    (cond
      ((< (elt clock player) 0)
       (format t "~&~c has no time left and forfeits."
               (name-of player))
       (THROW 'game-over (if (eql player black) -64 64)))
      ((eq move 'resign)
       (THROW 'game-over (if (eql player black) -64 64)))
      ((and (valid-p move) (legal-p move player board))
       (when print
         (format t "~&~c moves to ~a."
                 (name-of player) (88->h8 move)))
       (make-move move player board))
      (t (warn "Illegal move: ~a" (88->h8 move))
         (get-move strategy player board print clock)))))

(defun get-board (board)
  "Get the board"
  (loop for row from 1 to 8 collect
	(loop for col from 1 to 8
	      collect (bref board (+ col (* 10 row))))))
  
(defun print-board (&optional (board *board*) clock)
  "Print a board, along with some statistics."
  ;; First print the header and the current score
  (format t "~2&    a b c d e f g h   [~c=~2a ~c=~2a (~@d)]"
          (name-of black) (count black board)
          (name-of white) (count white board)
          (count-difference black board))
  ;; Print the board itself
  (loop for row from 1 to 8 do
        (format t "~&  ~d " row)
        (loop for col from 1 to 8
              for piece = (bref board (+ col (* 10 row)))
              do (format t "~c " (name-of piece))))
  ;; Finally print the time remaining for each player
  (when clock
    (format t "  [~c=~a ~c=~a]~2&"
            (name-of black) (time-string (elt clock black))
            (name-of white) (time-string (elt clock white)))))

(defun time-string (time)
  "Return a string representing this internal time in min:secs."
  (multiple-value-bind (min sec)
      (floor (round time internal-time-units-per-second) 60)
    (format nil "~2d:~2,'0d" min sec)))

(defun random-reversi-series (strategy1 strategy2
                              n-pairs &optional (n-random 10))
  "Play a series of 2*n games, starting from a random position."
  (reversi-series
    (switch-strategies #'random-strategy n-random strategy1)
    (switch-strategies #'random-strategy n-random strategy2)
    n-pairs))

(defun switch-strategies (strategy1 m strategy2)
  "Make a new strategy that plays strategy1 for m moves,
  then plays according to strategy2."
  #'(lambda (player board)
      (funcall (if (<= *move-number* m) strategy1 strategy2)
               player board)))

(defun reversi-series (strategy1 strategy2 n-pairs)
  "Play a series of 2*n-pairs games, swapping sides."
  (let ((scores
         (loop repeat n-pairs
             for random-state = (make-random-state)
             collect (reversi strategy1 strategy2 nil)
             do (setf *random-state* random-state)
             collect (- (reversi strategy2 strategy1 nil)))))
    ;; Return the number of wins (1/2 for a tie),
    ;; the total of the point differences, and the
    ;; scores themselves, all from strategy1's point of view.
    (values (+ (count-if #'plusp scores)
               (/ (count-if #'zerop scores) 2))
            (apply #'+ scores)
            scores)))

(defun round-robin (strategies n-pairs &optional
                    (n-random 10) (names strategies))
  "Play a tournament among the strategies.
  N-PAIRS = games each strategy plays as each color against
  each opponent.  So with N strategies, a total of
  N*(N-1)*N-PAIRS games are played."
  (let* ((N (length strategies))
         (totals (make-array N :initial-element 0))
         (scores (make-array (list N N)
                             :initial-element 0)))
    ;; Play the games
    (dotimes (i N)
      (loop for j from (+ i 1) to (- N 1) do
          (let* ((wins (random-reversi-series ; reversi-series
                         (elt strategies i)
                         (elt strategies j)
                         n-pairs n-random))
                 (losses (- (* 2 n-pairs) wins)))
            (incf (aref scores i j) wins)
            (incf (aref scores j i) losses)
            (incf (aref totals i) wins)
            (incf (aref totals j) losses))))
    ;; Print the results
    (dotimes (i N)
      (format t "~&~a~20T ~4f: " (elt names i) (elt totals i))
      (dotimes (j N)
        (format t "~4f " (if (= i j) '---
                             (aref scores i j)))))))
