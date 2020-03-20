;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests for the required functions in lab 2

(defparameter node-nevers
   (make-node :city 'Nevers) )
(defparameter node-paris
   (make-node :city 'Paris :parent node-nevers))
(defparameter node-nancy
   (make-node :city 'Nancy :parent node-paris))
(defparameter node-reims
   (make-node :city 'Reims :parent node-nancy))
(defparameter node-calais
   (make-node :city 'Calais :parent node-reims))

(defparameter node-calais-2
   (make-node :city 'Calais :parent node-paris))

(defparameter node-marseille-ex6
   (make-node :city 'Marseille :depth 12 :g 10 :f 20) )

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

(defparameter node-paris-ex7
  (make-node :city 'Paris :depth 0 :g 0 :f 20) )

(defparameter node-nancy-ex7
  (make-node :city 'Nancy :depth 2 :g 50 :f 50) )

(defparameter node-calais-ex7
  (make-node :city 'Calais :depth 3 :g 150 :f 0) )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 1:
;;
;;
(f-h 'Nantes *heuristic*) ;;  75.0 
(f-h 'Marseille *heuristic*) ;; 145.0
(f-h 'Lyon *heuristic*) ;; 105.0
(f-h 'Madrid *heuristic*)  ;; NIL
(f-h 'Nancy *heuristic*) ;; 50.0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 2:
;;
;;

(action-cost (car (navigate 'Avignon *trains*))) ;; 30.0
(mapcar #'action-cost (navigate 'Avignon *trains*)) ;; (30.0 16.0)
(action-final (car (navigate 'Avignon *trains*))) ;; LYON
(mapcar #'action-final (navigate 'Paris *trains*)) ;; (CALAIS NANCY NEVERS ORLEANS ST-MALO)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 3:
;;
;;

(f-goal-test node-calais '(Calais Marseille) '(Paris Limoges)) ;; NIL
(f-goal-test node-paris '(Calais Marseille) '(Paris)) ;; NIL
(f-goal-test node-calais '(Calais Marseille) '(Paris Nancy)) ;; T
(f-goal-test node-calais '(Calais Marseille) '()) ;; T

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 4:
;;
;;


(f-search-state-equal node-calais node-calais-2 '()) ;; T
(f-search-state-equal node-calais node-calais-2 '(Reims)) ;; NIL
(f-search-state-equal node-calais node-calais-2 '(Nevers)) ;; T
(f-search-state-equal node-nancy node-paris '()) ;; NIL
(f-search-state-equal node-calais node-calais '(Paris Nancy)) ;; T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 6:
;;
;; NOTE: In order to run the tests from this point on, you must 
;; have solved exercise 5, that is, you must have created the 
;; structure *travel* 
;;
;; ALSO NOTE: The output of tehse examples is "as is", the same as
;; it appears on the allegro console. The console "cuts" some of 
;; the outputs when these are too complex. Your output might be 
;; slightly diffferent than this one.

(node-g (car (expand-node node-marseille-ex6 *travel*))) ;; 26.0
(node-city (node-parent (car (expand-node node-marseille-ex6 *travel*)))) ;; MARSEILLE
(node-parent (node-parent (car (expand-node node-marseille-ex6 *travel*)))) ;; NIL
(mapcar #'(lambda (x) (node-city (node-parent x))) (expand-node node-nancy *travel*)) ;; (NANCY NANCY)
(node-g (car (expand-node node-marseille-ex6 *travel*))) ;; 26.0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 7:
;;


(defparameter sol-ex7 (insert-nodes-strategy (list node-paris-ex7 node-nancy-ex7) 
                                             (list node-calais-ex7)
                                             *A-star*))
(defparameter other-ex7 (insert-nodes-strategy sol-ex7 (list node-marseille-ex6) *A-star*)) 


(mapcar #'(lambda (x) (node-city x)) sol-ex7) ;; (CALAIS PARIS NANCY)
(mapcar #'(lambda (x) (node-g x)) sol-ex7) ;; (150 0 50)
(mapcar #'(lambda (x) (node-city x)) other-ex7) ;; (CALAIS PARIS MARSEILLE NANCY)
(mapcar #'(lambda (x) (node-g x)) other-ex7) ;; (150 0 10 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 8:
;;

(node-g (graph-search *travel* *A-star*)) ;; 202.0
(node-city (graph-search *travel* *A-star*)) ;; CALAIS
(node-city (node-parent (graph-search *travel* *A-star*))) ;; PARIS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 10:
;;
(solution-path NIL) ;; NIL
(solution-path (a-star-search *travel*)) ;; (MARSEILLE TOULOUSE LIMOGES ORLEANS PARIS CALAIS)
