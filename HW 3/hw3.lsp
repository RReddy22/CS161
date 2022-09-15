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
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

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

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((c (getKeeperColumn (car s) 0)))
	     (if c
		 ;keeper is in this row
		 (list row c)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
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
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

; helper function for goal-test that takes a row
; checks if there is at least one goal state in a row
(defun boxOrKeeperInRow (r)
	(if (null r) 
		nil
	(or (isBox (first r)) (isKeeper (first r)) (boxOrKeeperInRow (rest r)))))
  ;end defun

; goal-state takes a state and returns a boolean value
; if the state is null, then we are at the goal state 
; otherwise, we call boxOrKeeperInRow to check on the head of the state
; and then, we  recursively call goal-test on the remaining part of the state
(defun goal-test (s)
	(if (null s) 
		t
	(and (not (boxOrKeeperInRow (first s))) (goal-test (rest s)))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; helper function for get-square 
; takes a list and a number n
; returns the nth element in the list

 #| 
(defun nth (l n)
	(cond ((= 0 n) (first l))
	 ((null l) nil)
	(t (nth ((rest l) (- n 1))))))
|#

; helper function for next-states 
; takes in a State S, a row number r, and a column number c. 
; It should return the integer content of state S at square (r,c)
; If the square is outside the scope of the problem, return the value of a wall.
(defun get-square (S r c)
	(if (or (< r 0) (< c 0)) 
		wall
	(let ((v (first (nthcdr c (first (nthcdr r S)) ))))
	(if (null v) 
		wall
		v))))

; helper function for set-square 
; takes a list l, a number n, and a value v (content of a square)
; sets the nth element in list l to value v
(defun setNth (l n v)
	(if (= 0 n)
		(cons v (rest l))
	(cons (first l) (setNth (rest l) (- n 1) v))))
 
; helper function for next-states 
; takes in a state S, a row number r, a column number c, and a square content v (integer). 
; should return a new state S’ that is obtained by setting the square (r,c) to value v. 
; should not modify the input state.
(defun set-square (S r c v)
	(if (= r 0) 
		(cons (setNth (first S) c v) (rest S))
	(cons (first S) (set-square (rest S) (- r 1) c v))))

; use these constants for the try-move function
(setq down '(1 0))
(setq up '(-1 0))
(setq left '(0 -1))
(setq right '(0 1))

; helper function for try-move that removes a keeper
; takes only a State S
(defun removeKeeper (S)
	(let* ((pos (getKeeperPosition S 0))
		   (rKeeper (car pos))
	 	   (cKeeper (cadr pos))
		   (valueKeeper (get-square S rKeeper cKeeper)))
		(cond ((isKeeperStar valueKeeper) (set-square S rKeeper cKeeper star))
			  ((isKeeper valueKeeper) (set-square S rKeeper cKeeper blank)) 
			  (t 666))))


; helper function for try-move that adds a keeper
; takes only a state S
(defun addKeeper (S r c)
	(let ((v (get-square S r c)))
		(cond ((isStar v) (set-square S r c keeperstar))
			  ((isBlank v) (set-square S r c keeper)) 
			  (t 777))))

; helper function for try-move that steps to a square
(defun stepTo (S r c)
 (addKeeper (removeKeeper S) r c))

; helper function for try-move that checks if you can push a box
(defun cantPush (S r c D)
	(let* ((newR (+ r (car D)))
		   (newC (+ c (cadr D)))
		   (newV (get-square S newR newC)))
		(or (isBox newV) (isBoxStar newV) (isWall newV))))

; helper function for adding a box
(defun addBox (S r c)
	(let ((v (get-square S r c)))
		(cond ((isStar v) (set-square S r c boxstar))
			  ((isBlank v) (set-square S r c box)) 
			  (t 999))))

; helper function for removing a box
(defun removeBox (S r c)
	(let ((v (get-square S r c)))
		(cond ((isBoxStar v) (set-square S r c star))
			  ((isBox v) (set-square S r c blank)) 
			  (t 888))))
			  


; helper function for try-move 
; D is direction
; if we can NOT push, then we return NIL
; otherwise, we call removeBox, then addBox, then stepTp
(defun pushTo (S r c D)
	(if (cantPush S r c D) 
		NIL
	(let* ((dr (car D))
		   (dc (cadr D))
		   (newR (+ r dr))
		   (newC (+ c dc)))
		(stepTo (addBox (removeBox S r c) newR newC) r c))))

; takes in a state S and a move direction D
; returns the state that is the result of moving the keeper in state S in direction D. 
; NIL should be returned if the move is invalid (e.g. there is a wall in that direction). 
; update the content of every square to the right value.
(defun try-move (S D)
	(let* ((pos (getKeeperPosition S 0))
	 	   (r (car pos))
	 	   (c (cadr pos))
	 	   (dr (car D))
	       (dc (cadr D))
	       (newR (+ r dr))
	       (newC (+ c dc)) 
	       (v (get-square S newR newC)))
		(cond 
	 	    ((isWall v) NIL)
	 		((or (isBlank v) (isStar v)) (stepTo S newR newC))
	 		((or (isBox v) (isBoxStar v)) (pushTo S newR newC D)) )))

; next-states takes in a state s and we use cleanupList to remove NILs.
; we, then, create a list by calling try-move on each of our four directions.
; we defined these "directions" as constants 
(defun next-states (s)
	(cleanUpList (list (try-move s up) (try-move s right) (try-move s down) (try-move s left))))

	 ;(x (car pos))
	 ;(y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.


; EXERCISE: 
; returns the constant 0 and is a trivial admissible heuristic
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.

; this heuristic (h1) is NOT admissible because it
; does NOT check if the keeper is in the right spot, 
; so the keeper might not be on the goal when it says its done
(defun h1 (s)
  (if (null s)
	0
  (+ (count box (car s) :test #'equal) (h1 (cdr s)))))

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; test function that takes k and v and is used as a helper
; to the admissible h2 heuristic
(defun test (k v)
	(or (isBox v) (isKeeper v))
		1
	0)

; admissible heuristic that checks if the keeper is in the right spot, 
; so the keeper might will, in fact, be on the goal when it says its done
; this is unlike h1 which is not admissible and is essentially vice versa
(defun h2 (s)
  (if (null s)
	0
  (+ (count box (car s) :test #'test) (h1 (cdr s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq s1 '((1 1 1 1 1)
(1 4 0 0 1) (1 0 2 0 1) (1 0 3 0 1) (1 0 0 0 1) (1 1 1 1 1) ))

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

;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(41)
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

;(26)
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
