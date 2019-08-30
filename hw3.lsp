;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly effect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; effect your score).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.


; For reloading modified code.
; I found this easier than typing (load "filename") every time. 

(defun reload()
  (load "hw3.lsp")
  )

; For loading a-star.lsp.

(defun load-a-star()
  (load "a-star.lsp"))

; Reloads hw3.lsp and a-star.lsp

(defun reload-all()
  (reload)
  (load-a-star)
  )

; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).

(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

; Helper function of getKeeperPosition

(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (r c).
 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.

(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 (list row x)
		 (getKeeperPosition (cdr s) (+ row 1)))))))

; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).

(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)

; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; GOAL-TEST EXPLANATION
; my function recursively calls itself on each list element in state s. and calls count to
; determine if any 2s exist in the list (if we have reached a goal-state, then there should
; be zero 2s in the state s). if count returns anything other than 0, there are still 
; boxes in the state so we have not reached a goal state, else goal-state returns true 
; after iterating through each list in the state.

(defun goal-test (s)
 (cond ((null s) t)
       ((not (equal (count 2 (car s)) 0)) nil)
       ((not (equal (count 3 (car s)) 0)) nil)
       (t (goal-test (cdr s)))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.

; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.

; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...

; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

; GET-SQUARE EXPLANATION
; my function uses multiple nested nthcdr commands to extract the specific row and column 
; passed in to get-square. if this (row,col) returns null, then return 1 (it's a wall).
; else, use car and nthcdr to extract the square value

(defun get-square (s r c)
 (cond ((or (or (< r 0) (< c 0)) (null (car (nthcdr c (car (nthcdr r s)))))) 1)
       (t (car (nthcdr c (car (nthcdr r s)))))))

; SET-SQUARE EXPLANATION
; my function uses let to create an empty return state (so as to not modify the s passed in), 
; as well as define a variable for the total # of columns and rows in the current state.
; it then returns a list of the first r rows, and creates another list consisting of the rth row 
; and the first total_cols - c column values in the rth row with the changed (r,c) value to v and
; the last column values starting from c + 1, as well as the rest of the unchanged rows from the 
; initial state.
; this successfully changes only the square (r,c) to the value v, and leaves the rest of the 
; values alone while also not changing the input state. 

(defun set-square (s r c v)
 (let ((new_state nil)
       (cols (length (car s)))
       (rows (length s)))
      (append new_state 
       (cond ((equal r 0) 
	      (cons (append (butlast (car (nthcdr r s)) (- cols c)) (list v) (nthcdr (+ 1 c) 
		     (car (nthcdr r s)))) (cdr s)))
	     (t (append (butlast s (- rows r)) 
		 (cons (append (butlast (car (nthcdr r s)) (- cols c)) (list v) 
	                (nthcdr (+ 1 c) (car (nthcdr r s)))) (nthcdr (+ 1 r) s))))))))

; move directions passed in to try-move will be represented as follows:
; 0 -> up
; 1 -> down
; 2 -> left
; 3 -> right

; BOX/BOX-STAR/BLANK/STAR MOVE EXPLANATIONS
; these functions call set-square depending on the type of move taking place and set
; the respective positions to their new updated values.

(defun box-move (s v nkr nkc kr kc okr okc)
 (cond ((isKeeperStar (get-square s okr okc))
	(set-square (set-square (set-square s okr okc 4) kr kc 3) nkr nkc v))
       (t (set-square (set-square (set-square s okr okc 0) kr kc 3) nkr nkc v))))

(defun box-star-move (s v nkr nkc kr kc okr okc)
 (cond ((isKeeperStar (get-square s okr okc))
        (set-square (set-square (set-square s okr okc 4) kr kc 6) nkr nkc v))
       (t (set-square (set-square (set-square s okr okc 0) kr kc 6) nkr nkc v))))

(defun blank-move (s nkr nkc okr okc)
 (cond ((isKeeperStar (get-square s okr okc))
        (set-square (set-square s okr okc 4) nkr nkc 3))
       (t (set-square (set-square s okr okc 0) nkr nkc 3))))

(defun star-move (s nkr nkc okr okc)
 (cond ((isKeeperStar (get-square s okr okc))
        (set-square (set-square s okr okc 4) nkr nkc 6))
       (t (set-square (set-square s okr okc 0) nkr nkc 6))))

; MOVE-DIRECTION EXPLANATIONS
; 

(defun move (s d okr okc) 
 (let* ((nkr (cond ((equal d 0) (- okr 1))
	           ((equal d 1) (+ okr 1))
	           ((equal d 2) okr)
		   ((equal d 3) okr)
		   (t -1)))
	(nkc (cond ((equal d 0) okc)
		   ((equal d 1) okc)
		   ((equal d 2) (- okc 1))
		   ((equal d 3) (+ okc 1))
		   (t -1)))
        (ogr (cond ((equal d 0) (- nkr 1)) 
                   ((equal d 1) (+ nkr 1))
                   ((equal d 2) nkr)
                   ((equal d 3) nkr)
                   (t -1)))
        (ogc (cond ((equal d 2) (- nkc 1))
                   ((equal d 3) (+ nkc 1))
                   ((equal d 0) nkc)
                   ((equal d 1) nkc)
                   (t -1)))
 	(curr (cond ((or (< nkr 0) (< nkc 0)) nil)
	            (t (get-square s nkr nkc))))
	(next (cond ((or (< ogr 0) (< ogc 0)) nil)
	            (t (get-square s ogr ogc)))))
  (cond ((null curr) nil)
	((isBlank curr) (blank-move s nkr nkc okr okc))
        ((isStar curr) (star-move s nkr nkc okr okc))
        ((isBox curr)
         (cond ((null next) nil)
	       ((isBlank next) (box-move s 2 ogr ogc nkr nkc okr okc))
	       ((isStar next) (box-move s 5 ogr ogc nkr nkc okr okc))
	       (t nil)))
	((isBoxStar curr)
	 (cond ((null next) nil)
	       ((isBlank next) (box-star-move s 2 ogr ogc nkr nkc okr okc))
	       ((isStar next) (box-star-move s 5 ogr ogc nkr nkc okr okc))
	       (t nil)))
	(t nil))))
        
; TRY-MOVE EXPLANATION
; my function first sets kr and kc to the row and col position of the keeper using the helper
; function, and then depending on the direction integer, calls the appropriate move-d helper
; function (changing the keeper row or col depending on which direction is called) and returns
; whatever the move-d helper function returns

(defun try-move (s d)
 (let ((kp (getKeeperPosition s 0)))
  (move s d (car kp) (cadr kp))))

; NEXT-STATES EXPLANATION
; my function calls try-move in each direction and appends the lists of valid states returned
; from each of the try-move calls (also removes all of the unnecessary nils that may have been
; inserted from try-move calls) 

(defun next-states (s)
 (cleanUpList (append (cons (try-move s 0) 
		      (cons (try-move s 1) 
		      (cons (try-move s 2) 
		      (cons (try-move s 3) nil)))))))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; H0 EXPLANATION
; this function simply returns the constant 0 no matter what.

(defun h0 (s)
  0 )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; H1 EXPLANATION
; my function adds the number of 2s in each list of s by using count_boxes (helper function
; from goal-state implementation) and recursively calls itself on the rest of the lists in
; state s until reaching null (at which point it just adds 0) 

; IS THIS HEURISTIC ADMISSABLE?
; this heuristic is admissable because, while it does significantly over-simplify the cost
; of placing each box in a goal, it never overestimates the cost of placing each box in a goal.

(defun h1 (s)
 (cond ((null s) 0)
       (t (+ (count 2 (car s)) (h1 (cdr s))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 

; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; GOAL-COUNT EXPLANATION
; this function accepts a state and returns the number of empty goals, or goals with a keeper in it

(defun goal-count (s)
 (cond ((null s) 0)
       (t (+ (count 4 (car s)) (count 6 (car s)) (goal-count (cdr s))))))

; GETGOALCOLUMN EXPLANATION
; this function works similarly to getKeeperColumn in that it accepts a row and 
; a column # to start at and returns the column of a square if it is either a goal 
; or a goal with a keeper in it. else, nil.

(defun getGoalColumn (r col)
  (cond ((null (car (nthcdr col r))) nil)
        (t (if (or (isStar (car (nthcdr col r))) (isKeeperStar (car (nthcdr col r)))) col
            (getGoalColumn r (+ col 1))))))

; GETGOALPOSITION EXPLANATION
; this function works similarly to getKeeper Position in that it accepts a state, a row to begin
; with and a column to bgein with and returns the coordinate of a goal.

(defun getGoalPosition (s row col)
  (cond ((null s) nil)
        (t (let ((x (getGoalColumn (car (nthcdr 0 s)) col)))
             (if x
                 (list row x)
                 (getGoalPosition (nthcdr 1 s) (+ row 1) col))))))

; GET-GOALS EXPLANATION
; this function accepts a state, a number of goals, and a row and column to start at and uses
; the above two functions to return a list of every goal coordinate in the current state.

(defun get-goals (s ns goals row col)
  (cond ((> goals 1) 
	 (let* ((g (getGoalPosition ns row col)))
	  (cons g (append 
	   (cond ((null (getGoalColumn (car (nthcdr (car g) s)) (+ 1 (cadr g))))
	          (get-goals s (nthcdr (+ (car g) 1) s) (- goals 1) (+ 1 (car g)) 0))
	         (t (get-goals s (nthcdr (car g) s) (- goals 1) (car g) (+ 1 (cadr g)))))))))
        (t (list (getGoalPosition ns row col)))))

; BOX-COUNT EXPLANATION
; this function counts the number of boxes not on goals in the current state s

(defun box-count (s)
 (cond ((null s) 0)
       (t (+ (count 2 (car s)) (box-count (cdr s))))))

; GETBOXCOLUMN/GETBOXPOSITION/GET-BOXES EXPLANATION
; all three of these functions work exactly the same as their corresponding goal functions except
; they do so with boxes instead of goals.

(defun getBoxColumn (r col)
  (cond ((null (car (nthcdr col r))) nil)
        (t (if (or (isBox (car (nthcdr col r)))) col
            (getBoxColumn r (+ col 1))))))

(defun getBoxPosition (s row col)
  (cond ((null s) nil)
        (t (let ((x (getBoxColumn (car (nthcdr 0 s)) col)))
             (if x
                 (list row x)
                 (getBoxPosition (nthcdr 1 s) (+ row 1) col))))))

(defun get-boxes (s ns boxes row col)
  (cond ((> boxes 1)
         (let* ((b (getBoxPosition ns row col)))
          (cons b (append
           (cond ((null (getBoxColumn (car (nthcdr (car b) s)) (+ 1 (cadr b))))
                  (get-boxes s (nthcdr (+ (car b) 1) s) (- boxes 1) (+ 1 (car b)) 0))
                 (t (get-boxes s (nthcdr (car b) s) (- boxes 1) (car b) (+ 1 (cadr b)))))))))
        (t (list (getBoxPosition ns row col)))))

; COMPUTE-DISTANCE EXPLANATION
; this function accepts two coordinates (r1, c1) and (r2, c2) and computes the 
; manhattan distance between the two coordinates

(defun compute-distance (r1 c1 r2 c2)
 (let ((a (cond ((> r1 r2) (- r1 r2)) (t (- r2 r1))))
       (b (cond ((> c1 c2) (- c1 c2)) (t (- c2 c1)))))
  (+ a b)))

; KEEPER-TO-BOX EXPLANATION
; this function accepts a keeper position and list of boxes in the current state and
; returns the sum of the distance from the keeper to each box (each compute-distance value 
; is subtracted by 1 because we will later be adding this to the distance from each box
; to its closest goal)

(defun keeper-to-box (kp boxes)
 (cond ((null boxes) 0)
       (t (let ((kr (car kp)) (kc (cadr kp))
	        (br (caar boxes)) (bc (cadar boxes)))
	   (+ (- (compute-distance kr kc br bc) 1) 
	      (keeper-to-box kp (cdr boxes)))))))

; BOX-TO-GOAL EXPLANATION
; this function accepts a box coordinate and list of goals and 
; creates a list of each possible distance from the current box to each goal.

(defun box-to-goal (box goals)
 (cond ((null goals) nil)
       (t (let ((br (car box)) (bc (cadr box))
                (gr (caar goals)) (gc (cadar goals)))
           (append (list (compute-distance br bc gr gc)) 
	 	         (box-to-goal box (cdr goals)))))))

; MIN-DISTANCE EXPLANATION
; this function accepts the list of distances from box-to-goal and returns
; the smallest distance

(defun min-distance (minimum distances)
 (cond ((null distances) minimum)
       (t (cond ((<= minimum (car distances)) (min-distance minimum (cdr distances)))
	        (t (min-distance (car distances) (cdr distances)))))))

; SUM-BOX-DISTANCE EXPLANATION
; this function accepts a list of box and goal coordinates in the current state and returns
; a sum of the minimum distance from each box to its closest goal.

(defun sum-box-distance (boxes goals)
 (cond ((null boxes) 0)
       (t (let* ((lst (cleanUpList (box-to-goal (car boxes) goals)))) 
 	   (+ (min-distance (car lst) lst) (sum-box-distance (cdr boxes) goals))))))

; H604463201 EXPLANATION
; this function accepts a state and sets boxes to the list of boxes and goals to the list of goals
; (using get-boxes and get-goals). it then returns the sum of sum-box-distance and sum-keeper-distance
; in order to return the total cost of placing each box in a goal from keeper to box to goal.

(defun h604463201 (s)
 (let* ((kp (getKeeperPosition s 0))
	(boxes (cleanUpList (get-boxes s s (box-count s) 0 0)))
        (goals (cleanUpList (get-goals s s (goal-count s) 0 0))))
  (cond ((null boxes) 0)
  	(t (+ (sum-box-distance boxes goals) (keeper-to-box kp boxes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, 
 | (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution:
 | 
 | These numbers are located at the comments of the problems. For example, the first problem 
 | below has optimal solution depth 6.
 |
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. 
 | So, the depths of the optimal solutions provided could be used for checking whether your 
 | heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve 
 | without a good heuristic! 
 |#

;(6)
(setq p1 '((1 1 1 1 1 1)
           (1 0 3 0 0 1)
           (1 0 2 0 0 1)
           (1 1 0 1 1 1)
           (1 0 0 0 0 1)
           (1 0 4 0 4 1)
           (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
           (1 0 0 0 0 0 1)
           (1 0 0 0 0 0 1)
           (1 0 0 2 1 4 1)
           (1 3 4 0 1 0 1)
           (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
           (1 0 0 0 1 0 0 0 1)
           (1 0 0 0 2 0 3 4 1)
           (1 0 0 0 1 0 0 0 1)
           (1 0 4 0 1 0 0 0 1)
           (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
           (0 0 0 0 0 1 4)
           (0 0 0 0 0 0 0)
           (0 0 1 1 1 0 0)
           (0 0 1 0 0 0 0)
           (0 2 1 0 0 4 0)
           (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
           (1 1 0 0 1 1)
           (1 0 0 0 0 1)
           (1 4 2 2 4 1)
           (1 0 0 0 4 1)
           (1 1 3 1 1 1)
           (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
           (1 0 0 0 0 0 4 1)
           (1 4 0 0 2 2 3 1)
           (1 0 0 1 0 0 4 1)
           (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
           (0 0 1 1 1 1 4 0 0 3)
           (0 0 0 0 0 1 0 0 0 0)
           (0 0 0 0 0 1 0 0 1 0)
           (0 0 1 0 0 1 0 0 1 0)
           (0 2 1 0 0 0 0 0 1 0)
           (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
           (1 4 0 0 4 1)
           (1 0 2 2 0 1)
           (1 2 0 1 0 1)
           (1 3 4 0 4 1)
           (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1)
           (1 1 1 0 0 1 1 1 1)
           (1 0 0 0 0 0 2 0 1)
           (1 0 1 0 0 1 2 0 1)
           (1 0 4 4 4 1 3 0 1)
           (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
            (1 4 0 0 1 1 0)
            (1 3 2 0 0 1 1)
            (1 1 0 2 0 0 1)
            (0 1 1 0 2 0 1)
            (0 0 1 1 0 0 1)
            (0 0 0 1 1 4 1)
            (0 0 0 0 1 4 1)
            (0 0 0 0 1 4 1)
            (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
            (0 2 1 4 0 4 0)
            (0 2 0 4 0 0 0)
            (3 2 1 1 1 4 0)
            (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
            (1 0 0 4 1 0 0 0)
            (1 2 1 0 1 1 1 1)
            (1 4 0 0 0 0 0 1)
            (1 0 0 5 0 5 0 1)
            (1 0 5 0 1 0 1 1)
            (1 1 1 0 3 0 1 0)
            (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
            (1 3 0 0 1 0 0 4 4 1)
            (1 0 2 0 2 0 0 4 4 1)
            (1 0 2 2 2 1 1 4 4 1)
            (1 0 0 0 0 1 1 4 4 1)
            (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
            (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
            (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
            (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
            (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
            (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
            (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
            (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
            (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
            (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
            (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
            (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
            (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
            (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
            ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
            (1 1 1 0 0 1 1 1 1 0)
            (1 0 0 2 0 0 0 1 1 0)
            (1 3 2 0 2 0 0 0 1 0)
            (1 1 0 2 0 2 0 0 1 0)
            (0 1 1 0 2 0 2 0 1 0)
            (0 0 1 1 0 2 4 0 1 0)
            (0 0 0 1 1 1 1 0 1 0)
            (0 0 0 0 1 4 1 0 0 1)
            (0 0 0 0 1 4 4 4 0 1)
            (0 0 0 0 1 0 1 4 0 1)
            (0 0 0 0 1 4 4 4 0 1)
            (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
