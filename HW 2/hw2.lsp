; Overall Comment for the first three functions
; 
; 1. 
; BFS has a base case where if our tree (that's in a list representation) is null, then we return NIL
; We then check if the head of the list/tree is an atom. If it is, we put it at the front of the list 
; since it would be at the highest level and would be the first node visited in the search. If not, then
; we move it to the back. In either case, we run BFS on the rest of the rest, but in different ways
; 
; 2. 
; DFS has a base case where if our tree (that's in a list representation) is null, then we return NIL
; We have two cases similar to BFS, but we just switch the two things we are appending in each case and
; call DFS instead of BFS
;
; 3. 
; DF-IDS (Depth First Iterative Deepening Search) uses a helper function, 
; defined as Depth Limited Search. This does a Depth First Dearch, but with 
; a limited depth/height. This is what we call in DF-IDS and our base cases 
; would just be checking if the depth is <= 0. If it is, we return NIL. 
; If it's not, then we just do our standard recursive case with DF-IDS 
; where we append it to the DLS call

; BFS takes one argument which is a single tree (in a list representation)
; this function returns a single list that is the order of how the nodes would 
; be visited by left to right BFS search (shallowest node first)
(defun BFS (TREE)
    (cond ; like a switch statement
    ((null TREE) NIL) ; first case 
    ((atom (first TREE)) (append (list (car TREE)) (BFS (cdr TREE)))) ; second case
    (t (BFS (append (cdr TREE) (car TREE)))))) ; default case (t ensures there's a default case)

; DFS takes one argument which is a single tree (in a list representation)
; this function returns a single list that is the order of how the nodes would 
; be visited by left-to-right DFS search (deepest node first)
(defun DFS (TREE)
    (cond ; like a switch statement
    ((null TREE) NIL) ; first case 
    ((atom (first TREE)) (append (list (first TREE)) (DFS (cdr tree)))) ; second case
    (t (append (DFS (first tree)) (DFS(rest TREE)))))) ; default case (t ensures there's a default case)

; Depth Limited Search Helper Function
(defun DLS (TREE DEPTH)
    (cond ; like a switch statement
    ((or (null TREE) (<= DEPTH 0)) NIL) ; first case 
    ((atom (first TREE)) (append (DLS (cdr tree) DEPTH) (list (first TREE)))) ; second case
    (t (append (DLS (rest TREE) DEPTH) (DLS (first tree) (- DEPTH 1)))))) ; default case (t ensures there's a default case)

; DFID takes two arguments which are a single tree (in a list representation)
; and an integer represtnting the max depth 
; this function returns a list of terminal nodes in the order they would be visited
; in a right to left DFIDS
(defun DFID (TREE DEPTH)
    (if (<= DEPTH 0) NIL
        (append (DFID TREE (- DEPTH 1)) (DLS TREE DEPTH))))

; -----------------------------------------------------------------------------

; Implement a depth-first solver for the River-Boat problem that was described in class. To implement
; this solver, we provide you with a code skeleton (hw2_skeleton.lsp). Implement the functions in the code
; skeleton as described in their associated comments. DO NOT CHANGE THE FUNCTION NAMES OR
; PARAMETERS. Also do not write any additional functions.

; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal '(3 3 NIL) s))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ((firstXSide (- (car s) m)) 
        (first0Side (- (cadr s) c))
        (secondXSide (- 3 firstXSide))
        (second0Side (- 3 first0Side)))
    (cond ((or (> 0 firstXSide) (> 0 first0Side)) ())
          ((and (< firstXSide first0Side) (< 0 firstXSide)) ())
          ((and (< secondXSide second0Side) (< 0 secondXSide)) ())
          (t (list (list secondXSide second0Side (null (caddr s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.

(defun on-path (s states)
  (cond ((null states) ()) ; if states is NULL, then return empty list
    ((equal s (car states)) t) ; if s = the head of the states' stack, then return t
    (t (on-path s (cdr states)))))

    

  

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)

(defun mult-dfs (states path)
  (cond ((NULL states) ()) ; if states is NULL, then return NIL
    ((mc-dfs (car states) path) (mc-dfs (car states) path)) ; call the recursive case TWICE
	  (t (mult-dfs (cdr states) path)) ; default cause is to call the recursive function
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun mc-dfs (s path)
  (cond ((final-state s) (cons s path)) ; check the final state of s and then create a list with s and the path
       ((on-path s path) ()) ; check if s is in path
       (t (mult-dfs (succ-fn s) (cons s path))) ; default case: call successor function on s and then create a list like we did in the cond statement
  )
)

; Function execution examples 

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))


