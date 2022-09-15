;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; helper function that takes the assignmentList and a literal (lit) and returns true in two cases: 
; 1) if the head of assign is equal to literal 
; 2) if assignmentList is empty
; the recursive case calls satisifedLit on the rest of assignmentList
(defun satisifedLit (assignmentList lit) 
    (cond ((null assignmentList) t)
          ((= (first assignmentList) lit) t) 
          ((= 0 (+ (first assignmentList) lit)) nil)
          (t (satisifedLit (rest assignmentList) lit))
    )
)

; satisfiedClause takes two args: assignmentList and clause. 
; base case checks if the second arg is empty 
; function then checks if the clause is satisfied by calling the previous function and the current one recursively.
; c = clause
(defun satisfiedClause (assignmentList c) 
  (cond ((null c) nil)
        ((satisifedLit assignmentList (first c)) t)
        (t (satisfiedClause assignmentList (rest c)))
  )
)


; satisfiedClauses (this function) is pretty much the same as satisfiedClause (the last function)
; except it checks if the list of clauses is satisfiable by calling satisfiedClause and then satisfiedClauses, recursively.
; c = clauses
(defun satisfiedClauses (assignmentList c)
  (cond ((null c) t)
    (t (and (satisfiedClause assignmentList (first c)) (satisfiedClauses assignmentList (rest c))))
  )
)

; helper function used in backtrackSearch that appends an item from a list 
; takes two args: assignmentList and var
(defun AddAssignList (assignmentList var)
  (append assignmentList (list var))
)

; backtrackSearch takes in a number value "num", clauses, and an assignmentList
; this searches through the constraints and checks if we can go throw a certain path 
; if its valid, it will return the list. Otherwise, it does DFS to assign true or false 
; to the next variable that isn't at all assigned
; c = clauses
(defun backtrackSearch (num c assignmentList)
  (let* ((lenOfAssign (length assignmentList)))
  (cond ((satisfiedClauses assignmentList c)
        (cond ((= num (length assignmentList)) assignmentList)
              (t (or (backtrackSearch num c (AddAssignList assignmentList (- (length assignmentList) -1)))
                     (backtrackSearch num c (AddAssignList assignmentList (- 0 (- (length assignmentList) -1))))))
        ))
        (t nil)
  )
)

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta) 
  (backtrackSearch n delta nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

