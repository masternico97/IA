;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
  "Zero of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            x0:       initial estimation of the zero (seed)
            tol-abs:  tolerance for convergence


    OUTPUT: estimation of the zero of f, NIL if not converged"

  (unless (or (= max-iter 0) (< (abs (funcall df-dx x0)) 0.0001))
    (let ((x (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
      (if (< (abs (- x x0)) tol-abs) 
        x
        (newton f df-dx (- max-iter 1) x tol-abs)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
  "Zeros of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            seeds:    list of initial estimations of the zeros 
            tol-abs:  tolerance for convergence


    OUTPUT: list of estimations of the zeros of f"
  (mapcar #'(lambda (x) (newton f df-dx max-iter x tol-abs)) seeds))
  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-elt-lst (elt lst)
  "Combines an element with all the elements of a list


    INPUT:  elt: element 
            lst: list 


    OUTPUT: list of pairs, such that 
               the first element of the pair is elt. 
               the second element is an element from lst"
  (mapcar #'(lambda(x) (list elt x)) lst)) 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-lst-lst (lst1 lst2)
  "Calculate the cartesian product of two lists


    INPUT:  lst1: list 
            lst2: list 


    OUTPUT: list of pairs, of the cartesian product"
  (mapcar #'(lambda(x) (combine-elt-lst x lst2)) lst1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-list-of-lsts (lolsts)
  "Combinations of N elements, each of wich


   INPUT:  lstolsts: list of N sublists (list1 ... listN)


   OUTPUT: list of sublists of N elements, such that in each 
           sublist the first element is from list1
                 the second element is from list 2
                 ...
                 the Nth element is from list N"
  
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scalar-product (x y)
  "Calculates the scalar product of two vectors
 
   INPUT:  x: vector, represented as a list
           y: vector, represented as a list
 
   OUTPUT: scalar product between x and y


   NOTES: 
        * Implemented with mapcar"
  (reduce #'+ (mapcar #'(lambda (a b) (* a b)) x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm


(defun euclidean-norm (x)
  "Calculates the euclidean (l2) norm of a vector
   
    INPUT:  x: vector, represented as a list


    OUTPUT: euclidean norm of x"
(sqrt (reduce #'+ (mapcar #'(lambda (a) (* a a)) x))))


________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance


(defun euclidean-distance (x y) 
  "Calculates the euclidean (l2) distance between two vectors
 
    INPUT: x: vector, represented as a list
           y: vector, represented as a list


    OUTPUT: euclidean distance between x and y"
  (sqrt (reduce #'+ (mapcar #'(lambda (c) (* c c)) (mapcar #'(lambda (a b) (- b a)) x y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-similarity (x y) 
  "Calculates the cosine similarity between two vectors


    INPUT:  x: vector, representad as a list
            y: vector, representad as a list


    OUTPUT: cosine similarity between x and y


    NOTES: 
       * Evaluates to NIL (not defined)
         if at least one of the vectors has zero norm.
       * The two vectors are assumed to have the same length"
  (unless (or (null x)  (null y))
    (/ (scalar-product x y) (* (euclidean-norm x) (euclidean-norm y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angular-distance (x y) 
  "Calculates the angular distance between two vectors


   INPUT:  x: vector, representad as a list
           y: vector, representad as a list


   OUTPUT: cosine similarity between x and y


   NOTES: 
      * Evaluates to NIL (not well defined)
        if at least one of the vectors has zero norm.
      * The two vectors are assumed to have the same length"
  (unless (or (null x)  (null y))
  (/ (acos (cosine-similarity x y)) pi)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors


(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    "Selects from a list the vectors whose similarity to a 
     test vector is above a specified threshold. 
     The resulting list is ordered according to this similarity.
 
     INPUT:  lst-vectors:   list of vectors
             test-vector:   test vector, representad as a list
             similarity-fn: reference to a similarity function
             threshold:     similarity threshold (default 0)
      
     OUTPUT: list of pairs. Each pair is a list with
             a vector and a similarity score.
             The vectors are such that their similarity to the 
             test vector is above the specified threshold.
             The list is ordered from larger to smaller 
             values of the similarity score 
     
     NOTES: 
        * Uses remove-if and sort"
  (unless (or (null lst-vectors)  (null test-vector))
    (sort
      (remove-if #'(lambda (y) (null y))
        (mapcar #'(lambda (x)
          (let 
            ((similarity (funcall similarity-fn x test-vector)))
            (unless (< similarity threshold)
              (list x similarity)))) lst-vectors))
      #'(lambda(a b) (> (abs a) (abs b)))
        :key #'second)))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


________________


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
  "Selects from a list the vector that is closest to the 
   reference vector according to the specified distance function 
 
   INPUT:  lst-vectors:   list of vectors
           ref-vector:    reference vector, represented as a list
           distance-fn:   reference to a distance function
      
   OUTPUT: List formed by two elements:
           (1) the vector that is closest to the reference vector 
               according to the specified distance function
           (2) The corresponding distance value.


   NOTES: 
      * The implementation is recursive
      * It ignores the vectors in lst-vectors for which the 
        distance value cannot be computed."
   (unless (or (null lst-vectors)  (null test-vector))
    (first (sort (
    		(remove-if #'(lambda (y) (null y))
				(list 
					(let ((distance (funcall distance-fn lst-vectors test-vector) ))
					)
				)
			)
    	) 
    	
    	#'(lambda(a b) (< (abs a) (abs b))) :key #'second
    ))
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun backward-chaining (goal lst-rules)
  "Backward-chaining algorithm for propositional logic
 
   INPUT: goal:      symbol that represents the goal
          lst-rules: list of pairs of the form 
                     (<antecedent>  <consequent>)
                     where <antecedent> is a list of symbols
                     and  <consequent> is a symbol


   OUTPUT: T (goal derived) or NIL (goal cannot be derived)


   NOTES: 
        * Implemented with some, every" 


  (backward-chaining-aux goal lst-rules NIL))










________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) 
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end) 
          (reverse path)
        (bfs end 
             (append (rest queue) 
                     (new-paths path node net)) 
             net))))) 


 (defun new-paths (path node net)
  (mapcar #'(lambda(n) 
        (cons n path)) 
                (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shortest-path (start end net)
  (bfs end (list (list start)) net))    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bfs-improved (end queue net)
  )




(defun shortest-path-improved (end queue net)
  )
