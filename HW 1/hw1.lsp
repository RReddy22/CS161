; Overall Comment Explaining Solutions
;
; 1. 
; This treutrns true is N is in the tree and NIL otherwise 
; We recursively check each tree (meaning the first second then third trees) 
; until we get to an atom in which case we can use our base case
; to see if it is = N
; 
; 2.
; We recursively call the function until reaching an atom like the last function, 
; but we do it on the left subtree since wew know we want the minimum element 
;
; 3.
; We recursovely order the middle left and right tree and append them
;
; 4.
; If the beginning is > 0, we recursively call it on the rest of L where we decrement start by 1
;
; 5.
; check if the length is odd or even, then use the SUB-LISt function for the appropriate one (odd or even)
;
; 9.
; If E1 AND E2 are null, then we want to return true
; If both are atoms, we want to compare them 
; If one is NULL, then return NIL
; For the recrusive case, we want to call the function on itself using "car" and "cdr" for each list
;
; 10.
; NOT DONE

; START OF CODE DOWN BELOW
; Above Each Function is a header comment explaining precisely what its arguments are, 
; and what value it returns in terms of its arguments

; 1st argument is "N" and it is what we are looking for in the tree 
; 2nd argument is the TREE which is an ordered tree that we are looking through for N
; this returns a boolean value (t or NIL) based on whether we can find "N" in TREE
; 2nd line is the base case when the TREE is empty or NULL
(defun TREE-CONTAINS (N TREE)
(cond ((null TREE) nil)             
    ((atom TREE) (cond ((= N TREE) T)(T nil)))   
        (T (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (second TREE)) (TREE-CONTAINS N (third TREE))))))

; the only argument is the ordered tree we have 
; this returns the maximum number in the tree
(defun TREE-MAX (TREE)
(cond ((null TREE) nil)
    ((null (rest TREE)) (TREE-MAX (first TREE)))
        (T (TREE-MAX (rest TREE)))))

; the only argument is the ordered tree as input
; this function returns a list in a "post order" 
(defun TREE-ORDER (TREE)
    )   

; the first argument is a list L
; the second argument is an integer that is >= 0 labeled as "START"
; the third argument is an integer that is >= 0 labeled as "LEN"
; this function returns a sub-list of L (the original ist) that starts at position "START" and goes for length "LEN"
(defun SUB-LIST (L START LEN)
(cond ((null L) nil)
    ((= LEN 0) nil)
    ((= START 0) (cons (car L) (SUB-LIST (cdr L) 0 (- LEN 1))))
    (T (SUB-LIST (cdr L) (- START 1) LEN))))

; the only argument is a list L
; this function returns 2 lists L1 and L2 such that when you append them to each other, they create the original list
; since these are split up, the result of subtracting their lengths will be 1 or 0 depending on if L is odd or even, respectively
(defun SPLIT-LIST (L)
(let* ((sizeOfList (length L)))
    (cond ((oddp sizeOfList) (list (SUB-LIST L 0 (* (+ 1 sizeOfList) 0.5)) (SUB-LIST L (* (+ 1 sizeOfList) 0.5) (/ (- sizeOfList 1) 2))))
        (T (list (SUB-LIST L 0 (* sizeOfList 0.5)) (SUB-LIST L (* sizeOfList 0.5) (* sizeOfList 0.5)))))))

; only argument is a binary tree named TREE
; this function returns the height of this binary tree which is defined as the
; length of the longest path from the root to the leaf node that is farthest away 
(defun BTREE-HEIGHT (TREE)
    (cond ((null TREE) 0)
    ((atom TREE) 0)
    ((listp TREE) 
        (cond (( < (BTREE-HEIGHT (second TREE)) (BTREE-HEIGHT (car TREE))) (+ (BTREE-HEIGHT (car TREE)) 1))
            (t (+ (BTREE-HEIGHT (second TREE)) 1))))
            (t (* (BTREE-HEIGHT (rest TREE)) 1))))

; input is a list that is non emmpty and that contains atoms
; this function should return a binary tree of the form where: 
; the tree leaves are elements of the input/argument (LEAVES)
; also, the difference in the # of leaves in a non-leaf node's left branch 
; and its right branch should be 0 or 1 and nothing else. 
(defun LIST2BTREE (LEAVES)
    (cond ((null LEAVES) NIL) 
        ((= (length LEAVES) 1) (first LEAVES))
        ((= (length LEAVES) 2) LEAVES)
        (T (list (LIST2BTREE (car (SPLIT-LIST LEAVES)))
                 (LIST2BTREE (second (SPLIT-LIST LEAVES)))))))

; only input/arg is a binary tree called TREE where each node has <= 2 children 
; this function returns atoms in a list (or a list of atoms) 
(defun BTREE2LIST (TREE)
(cond ((null TREE) nil)
    ((atom TREE) (list TREE))
    (T (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))

; first argument is E1 which is our first LISP expression 
; second argument is E2 which is our second LISP expression
; this function returns true if the lists are equal and false otherwise
(defun IS-SAME (E1 E2)
    (cond ((and (null E1)(null E2)) t)
        ((and (not (null E1))(null E2)) NIL)
        ((and (not (null E2))(null E1)) NIL)
        ((and (atom E1) (atom E2))(= E1 E2))
        ((and (listp E1) (listp E2))(and (IS-SAME (car E1)(car E2))(IS-SAME (cdr E1) (cdr E2)))) (T NIL)))

; first argument is E1 which is our first LISP expression 
; second argument is E2 which is our second LISP expression
; this function returns a list that is the result of appending E2 to E1 
; in a DFS occurence order (in terms of E2's atoms/elements) 
(defun FLATTEN-APPEND (E1 E2)
    )
    
    



    
    