; function #1
; input: a number, n, and an ordered tree, tree
; output: t if n exists in tree or nil if n does not exist in tree

; function #1 explanation
; my function accepts a number n and a tree and checks if n exists in the tree by
; returning nil if the tree is empty or returning true if the first element of the
; tree is equal to n or recursively calling itself and passing in the tree without
; the first element since we know it is not equal to n.
; if the first element of the tree is a list in itself, the function recursively
; calls itself and passes in a new list with the first element appended to the rest
; of the elements of the tree.
; once the function finds n in the tree it returns true, else returns nil

(defun tree-contains (n tree)
 (cond ((null tree) nil)
       ((equal (car tree) n) t)
       ((equal (listp (car tree)) t) (tree-contains n (append (car tree) (cdr tree))))
       (t (tree-contains n (cdr tree)))))

; function #2
; input: an ordered tree, tree
; output: a number such that it is the minimum number from the tree

; function #2 explanation
; my function accepts an ordered tree--if the first element of the tree is a list,
; it recursively calls itself and passes in the first element of the tree (the nested list).
; else, it returns the first element of the tree since the leftmost element in an ordered
; tree will always be the minimum of the tree

(defun tree-min (tree)
(cond ((null tree) nil)
      ((equal (listp (car tree)) t) (tree-min (car tree)))
      (t (car tree))))

; function #3
; input: an ordered tree, tree
; output: a pre-ordered list of numbers from tree

; function #3 explanation
; my function accepts an ordered tree with multiple base cases depending on what is passed in--
; 1. if an element is passed in that is not a list, it will return the item as a list (so the
; element can be appended); 2. if a one-element list is passed in (e.g., (1) or (3)) then it will
; return the element as a list; 3. lastly, the function recursively calls itself and creates a 
; new list by appending the root, left node, and right node of the original list

(defun tree-order (tree)
 (cond ((equal (numberp tree) t) (list tree))
       ((and (equal (length tree) 1) (equal (numberp (car tree)) t)) (list (car tree)))
       (t (append (list (cadr tree)) (tree-order (car tree)) (tree-order (caddr tree))))))

; function #4
; input: a list, l, and two non-negative integers, start and len
; output: a sublist of l where the first element is start and the length of the sublist is len

; function #4 explanation
; my function takes in a tree, start #, and length # and if length = 0, returns nil--else,
; it returns the first item of the list if start = 0 and length = 1.
; if start = 0 and length > 1, then it appends the first item of the list with the return value
; from a recursive call that passes in the rest of the list, the same start value, and decrements
; the len value.
; if start > 0, then it appends the return value from a recursive call that passes in the rest
; of the list and decrements the start value (keeping len the same) until start = 0 and l begins
; with the correct start element

(defun sub-list (l start len)
 (cond ((equal len 0) nil)
       ((and (equal start 0) (equal len 1)) (list (car l)))
       ((equal start 0) (append (list (car l)) (sub-list (cdr l) start (- len 1))))
       (t (append (sub-list (cdr l) (- start 1) len)))))

; function #5
; input: a list, l
; output: a list of two lists, l1 and l2, such that l is the appended version of l1 and l2
;         and the lengths of l1 and l2 differ by 0 or 1

; function #5 explanation
; my function takes a list and sets the variable len to the length of the list.
; if len = 2, it returns a list of two lists with each element as a list.
; if len is odd, then the function creates a list by calling sub-list twice with 0 and (len+1)/2
; and (len+1)/2 and (len-1)/2. this successfully splits the list into two sub-lists where the
; length of the first sub-list is always one more than the length of the second sub-list.
; if len is even, then the function creates a list by calling sub-list twice with 0 and (len/2) and
; (len/2) and (len/2).

(defun split-list (l)
 (let ((len (length l)))
  (cond ((equal len 2) (list (list (car l)) (cdr l)))
        ((equal (oddp len) t) 
         (list (sub-list l 0 (/ (+ len 1) 2)) (sub-list l (/ (+ len 1) 2) (/ (- len 1) 2))))
        (t (list (sub-list l 0 (/ len 2)) (sub-list l (/ len 2) (/ len 2))))))) 

; function #6
; input: a binary tree, tree
; output: a number that corresponds to the height of tree

; function #6 explanation
; my function takes in a binary tree and returns 0 if the tree is empty or if the tree is only
; one element; else the function recursively calls itself and passes in the left and right
; sub-trees of the original tree and incrementally adds one with each function call.
; it returns the maximum of the two sub-tree values as the height of the original tree.

(defun btree-height (tree)
 (cond ((null tree) 0)
       ((atom tree) 0)
       (t (+ 1 (if (>= (btree-height (cadr tree)) (btree-height (caddr tree)))
	           (btree-height (cadr tree))) (btree-height (caddr tree))))))

; function #7
; input: a non-empty list of atoms, leaves
; output: a binary tree such that its leaves are the elements from the input leaves and
;         the number of leaves in each branch of each internal node differs by 0 or 1

; function #7 explanation
; my function takes in a list of elements and returns the element if it is a one-element list;
; else, it returns the list if it is a 2 element list.
; for all other cases, it splits the list of leaves and recursively calls itself, creating a list
; consisting of the return values of list2btree on the first half of the value of split-list and 
; the return values of list2btree on the second half of the value of split-list.

(defun list2btree (leaves)
 (cond ((equal (length leaves) 1) (car leaves))
       ((equal (length leaves) 2) leaves)
       (t (list (list2btree (car (split-list leaves))) (list2btree (cadr (split-list leaves)))))))

; function #8
; input: a binary tree, tree
; output: list of atoms

; function #8 explanation
; my function takes a binary tree and if it is a one-element tree, it returns the element as a
; one-element list; else if the tree is two atoms long, it returns the tree;
; else, for all other cases it recursively calls itself on the first half of the tree and the 
; second half of the tree and appends those two results together to form one list.

(defun btree2list (tree)
 (cond ((atom tree) (list tree))
       ((and (equal (length tree) 2) (atom (car tree))) tree)
       (t (append (btree2list (car tree)) (btree2list (cadr tree)))))) 

; function #9
; input: two lisp expressions, e1 and e2, whose atoms are all numbers
; output: returns t if expressions are identical and nil if not

; function #9 explanation
; my function accepts two lisp expressions and returns true if both are null.
; else, if both are atoms and both are equal, it returns true.
; else, it checks if both are lists and if so, recursively calls itself returning true if 
; both the cars of e1 and e2 are the same and both the cdrs of e1 and e2 are the same.
; else, it returns nil

(defun is-same (e1 e2)
 (cond ((and (null e1) (null e2)) t)
       ((and (and (atom e1) (atom e2)) (= e1 e2)) t)
       ((and (listp e1) (listp e2))
        (and (is-same (car e1) (car e2)) (is-same (cdr e1) (cdr e2))))
       (t nil)))
