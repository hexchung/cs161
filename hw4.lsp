; IS-COMPLETE-SOL
; this function accepts a value var from a clause in delta and a solution list sol.
; it returns false if var and an element in sol conflict. else, true.

(defun is-complete-sol (sol var)
 (cond ((null sol) t)
       ((equal (car sol) var) t)
       ((equal (car sol) (- var)) nil)
       (t (is-complete-sol (cdr sol) var))))

; IS-COMPLETE-CLAUSE EXPLANATION
; helps is-complete iterate through each clause in delta to check if sol satisfies an element
; in the clause. 

(defun is-complete-clause (sol clause)
 (cond ((null clause) nil)
       ((is-complete-sol sol (car clause)))
       (t (is-complete-clause sol (cdr clause)))))

; IS-COMPLETE EXPLANATION
; this function accepts a solution list sol and a list of clauses delta.
; if delta is null, then return true. else, use helper functions to iterate through
; each clause in delta and check if all clauses are satisfied by the given solution list sol.

(defun is-complete (sol delta)
 (cond ((null delta) t)
       ((is-complete-clause sol (car delta)) (is-complete sol (cdr delta)))
       (t nil)))

; CHECK-SOL EXPLANATION
; this function accepts the number of variables n (original) and var (to count down), a 
; solution list sol, and a lits of clauses delta. if is-complete sol delta returns true and 
; if sol contains values for all variables n, then it returns the solution list. else, if 
; sol does not contain all values for all variables yet, it calls check-sol twice, decrements
; our counter var variable, and tries to find a solution for both the positive variable var
; and the negative variable var and returns a solution list for whichever returns true.

(defun check-sol (n vars sol delta)
 (cond ((is-complete sol delta) 
	(cond ((equal (length sol) n) sol)
	      (t (or (check-sol n (- vars 1) (append sol (list vars)) delta)
 	             (check-sol n (- vars 1) (append sol (list (- vars))) delta)))))
       (t nil)))

; SAT? EXPLANATION
; this function accepts the number of variables n and a list of clauses delta.
; it declares an empty list sol to be later filled in with satisfying variables by check-sol.
; it then returns whatever check-sol returns.

(defun sat? (n delta)
 (let ((sol nil))
  (check-sol n n sol delta)))
