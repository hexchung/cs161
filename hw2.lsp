; function #1
; input: a tree in list representation
; output: one list of nodes (atoms) in order of a right-to-left depth first search

; function #1 explanation
; my function takes in a tree in list representation and 1. returns nil if the tree passed 
; in is empty; 2. returns the atom in list form if the tree passed in is just an atom; 
; and 3. else recursively calls itself from the end of the list to the front (since the 
; depth first search is right-to-left) and appends the return values of the recursive calls.

(defun dfs (tree)
 (cond ((null tree) nil)
       ((atom tree) (list tree))
       (t (append (dfs (cdr tree)) (dfs (car tree))))))

; function #2
; input: a tree in list representation
; output: one list of nodes (atoms) in order of a right-to-left depth first iterative 
; deepening search.

; function #2 explanation
; my function recursively calls itself while decrementing in order to call my dls helper function
; exactly n times. dfid appends the results from dls, where dls either returns nil or appends
; the atoms of tree in order of an iterative deepening search.

(defun dls (tree lim)
 (cond ((null tree) nil)
       ((atom tree) (list tree))
       ((equal lim 0) nil)
       (t (append (dls (cdr tree) lim) (dls (car tree) (- lim 1))))))
  
(defun dfid (tree n)
 (cond ((equal n 0) nil)
       (t (append (dfid tree (- n 1)) (dls tree n)))))

; function #3
; input: a state s
; output: t if s is equal to the goal state (3 3 nil), nil otherwise

; function #3 explanation
; this function simply checks if the input state is equal to (3 3 nil) and returns nil otherwise

(defun final-state (s)
 (cond ((equal s '(3 3 nil)) t)
       (t nil)))

; function #4
; input: a state s, a # of missionaries to move, and a # of cannibals to move
; output: a one-element list of the valid state that results from moving m missionaries and c
;         cannibals, nil if the operation is invalid or results in an invalid state 

; function #4 explanation
; my function first checks to make sure we are only attempting to move 1 or 2 people at a time and
; returns nil otherwise; else it checks that we are only attempting to move a valid number of 
; missionaries/cannibals on one side (can't move 3 missionaries if there are only 2 on this side);
; else, it executes the operation creating two states, curr and next, that are the result of 
; moving m missionaries and c cannibals to the other side. 
; finally, it checks if either side has more cannibals than missionaries and if true checks if
; there are zero missionaries and returns nil otherwise (invalid state).
; if all is true, then the function returns a one-element list of the valid next state.

(defun next-state (s m c)
 (cond ((or (> (+ m c) 2) (<= (+ m c) 0)) nil)
       ((> m (first s)) nil)
       ((> c (second s)) nil)
       (t (let* ((curr (list (- (first s) m) (- (second s) c) (third s)))
                 (next (list (- 3 (first curr)) (- 3 (second curr))
                        (cond ((equal (third curr) t) nil) (t t)))))
           (cond ((and (> (second curr) (first curr)) (not (equal (first curr) 0))) nil)
                 ((and (> (second next) (first next)) (not (equal (first next) 0))) nil)
                 (t (list next)))))))

; function #5
; input: a state s
; output: a list of valid next states beginning from state s

; function #5 explanation
; since there are only five possible combinations of moving people given the restrictions, 
; my function appends the results of calling next-state using s as the starting state, and going
; through each possible combination of moving the missionaries and cannibals. 
; it returns a list of the next-states that were valid.

(defun succ-fn (s)
 (cond ((null s) nil)
       (t (append (cond ((not (equal (next-state s 0 1) nil)) (next-state s 0 1)) (t nil))
                  (cond ((not (equal (next-state s 1 0) nil)) (next-state s 1 0)) (t nil))
                  (cond ((not (equal (next-state s 1 1) nil)) (next-state s 1 1)) (t nil))
                  (cond ((not (equal (next-state s 2 0) nil)) (next-state s 2 0)) (t nil))
                  (cond ((not (equal (next-state s 0 2) nil)) (next-state s 0 2)) (t nil))))))

; function #6
; input: a state s and a list of states states
; output: t if s is in states and nil otherwise

; function #6 explanation
; my function recursively calls itself and iterates through the list of states checking if s is
; equal to any of the elements of states. if so, it returns true, else nil.

(defun on-path (s states)
  (cond ((null states) nil)
         ((equal s (car states)) t)
         (t (on-path s (cdr states)))))

; function #7
; input: a set of valid next-states from the last state in path (states), and a first-in first-out
;        list of states that have been visited from initial state to current state (path)
; output: a list of states from intial state to goal state, or nil if there is no solution

; function #7 explanation
; my function checks cases in which states is empty or the first element of states has already
; been visited; if visited already, it recursively calls itself on the rest of the list states.
; else, if the first element of states is the final state, it appends the final state to path
; and returns path.
; else, it appends the return values of non-nil recursive calls to itself, passing in consecutive
; valid next-states as input and appending visited states to path as it recurses.

(defun mult-dfs (states path)
 (cond ((null states) nil)
       ((on-path (car states) path) (mult-dfs (cdr states) path))
       ((final-state (car states)) (append path (list (car states))))
       (t (append (cond ((equal (mult-dfs (succ-fn (car states)) (append path (list (car states))))
                          nil) (mult-dfs (cdr states) path))
                        (t (mult-dfs (succ-fn (car states)) (append path (list (car states))))))))))

; function #8
; input: a state s and a list of visited states from initial to current, or nil if s is initial state
; output: a list of states from initial state to goal state, or nil if there is no solution

; function #8 explanation
; my function checks whether s is the final state, and if so, returns path; else, it checks if path
; is nil, if so it means s is the start state so the function calls mult-dfs and passes in valid 
; next-states from the intial state s and path containing the initial state.
; else, s is not the initial state and path contains visited states so it calls mult-dfs and passes in
; valid next-states from s and path.

(defun mc-dfs (s path)
 (cond ((final-state s) path)
       ((null path) (mult-dfs (succ-fn s) (list s)))
       (t (mult-dfs (succ-fn s) path))))
