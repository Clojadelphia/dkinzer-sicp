(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 2.2.1 Representing Sequences
;
; {{{2 Exercise 2.17:
; {{{3 Problem
;      Define a procedure `last-pair` that returns the
;      list that contains only the last element of a given (nonempty)
;      list:
;
;           (last-pair (list 23 72 149 34))
;           (34)
;
; {{{3 Solution
(define (last-pair a-list)
  (if (null? (cdr a-list))
    (car a-list)
    (last-pair (cdr a-list))))
(assert (= 34 (last-pair (list 23 72 149 34))) "Last pair of the list is 34.")

; {{{2 Exercise 2.18:
; {{{3 Problem
;      Define a procedure `reverse' that takes a list as
;      argument and returns a list of the same elements in reverse order:
;
;           (reverse (list 1 4 9 16 25))
;           (25 16 9 4 1)
;
; {{{3 Solution
(define (reverse a-list)
  (if (null? a-list)
    a-list
    (append (reverse (cdr a-list)) (list (car a-list)))))

(assert (equal? (reverse (list 1 4 9 16 25))
                (list 25 16 9 4 1))
        "The reverse procedure reverses a list.")

; {{{2 Exercise 2.19:
; {{{3 Problem
;      Consider the change-counting program of section
;      *Note 1-2-2::.  It would be nice to be able to easily change the
;      currency used by the program, so that we could compute the number
;      of ways to change a British pound, for example.  As the program is
;      written, the knowledge of the currency is distributed partly into
;      the procedure `first-denomination' and partly into the procedure
;      `count-change' (which knows that there are five kinds of U.S.
;      coins).  It would be nicer to be able to supply a list of coins to
;      be used for making change.
;
;      We want to rewrite the procedure `cc' so that its second argument
;      is a list of the values of the coins to use rather than an integer
;      specifying which coins to use.  We could then have lists that
;      defined each kind of currency:
;
(define us-coins (list 50 25 10 5 1))
;
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;
;      We could then call `cc' as follows:
;
;           (cc 100 us-coins)
;           292
;
;      To do this will require changing the program `cc' somewhat.  It
;      will still have the same form, but it will access its second
;      argument differently, as follows:
;
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

;      Define the procedures `first-denomination',
;      `except-first-denomination', and `no-more?' in terms of primitive
;      operations on list structures.  Does the order of the list
;      `coin-values' affect the answer produced by `cc'?  Why or why not?

(assert (= 292 (cc 100 us-coins))
        "The new #cc procedure works as expected.")

(assert (= (cc 100 (reverse us-coins)) (cc 100 us-coins))
        "The order of the coins does not matter becase the recursive nature of the
        cc procedure gurantees that all combinations will be exhausted before on evaluation
        regardless of the order of the list.")
; {{{3 Solution
; {{{2 Exercise 2.20:
; {{{3 Problem
;      The procedures `+', `*', and `list' take
;      arbitrary numbers of arguments. One way to define such procedures
;      is to use `define' with notation "dotted-tail notation".  In a
;      procedure definition, a parameter list that has a dot before the
;      last parameter name indicates that, when the procedure is called,
;      the initial parameters (if any) will have as values the initial
;      arguments, as usual, but the final parameter's value will be a "list"
;      of any remaining arguments.  For instance, given the definition
;
;           (define (f x y . z) <BODY>)
;
;      the procedure `f' can be called with two or more arguments.  If we
;      evaluate
;
;           (f 1 2 3 4 5 6)
;
;      then in the body of `f', `x' will be 1, `y' will be 2, and `z'
;      will be the list `(3 4 5 6)'.  Given the definition
;
;           (define (g . w) <BODY>)
;
;      the procedure `g' can be called with zero or more arguments.  If we
;      evaluate
;
;           (g 1 2 3 4 5 6)
;
;      then in the body of `g', `w' will be the list `(1 2 3 4 5 6)'.(4)
;
;      Use this notation to write a procedure `same-parity' that takes
;      one or more integers and returns a list of all the arguments that
;      have the same even-odd parity as the first argument.  For example,
;
;           (same-parity 1 2 3 4 5 6 7)
;           (1 3 5 7)
;
;           (same-parity 2 3 4 5 6 7)
;           (2 4 6)
;
; {{{3 Solution
(define (same-parity x . a)
  ; This is a bit ugly because the problem forces me to add x to the result.
  (define (same-parityer x . a)
    (if (null? a)
      a
      (if (or (and (even? x)
                   (even? (car a)))
              (and (odd? x)
                   (odd? (car a))))
        (append (list (car a)) (apply same-parityer  x (cdr a)))
        (append '() (apply same-parityer x (cdr a))))))
  (cons x (apply same-parityer x a)))

(assert (equal? (list 1 3 5 7)
                (same-parity 1 2 3 4 5 6 7))
        "#same-parity works as expected.")

; {{{2 Exercise 2.21:
; {{{3 Problem
;      The procedure `square-list' takes a list of
;      numbers as argument and returns a list of the squares of those
;      numbers.
;
;           (square-list (list 1 2 3 4))
;           (1 4 9 16)
;
;      Here are two different definitions of `square-list'.  Complete
;      both of them by filling in the missing expressions:
;
;           (define (square-list items)
;             (if (null? items)
;                 nil
;                 (cons <??> <??>)))
;
;           (define (square-list items)
;             (map <??> <??>))
;
; {{{3 Solution
(define nil '())

(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items)) (square-list (cdr items)))))

(assert (equal? (list 1 4 9 16) (square-list (list 1 2 3 4)))
        "The first #square-list procedure works as expected.")

(define (square-list items)
  (map square (list 1 2 3 4)))

(assert (equal? (list 1 4 9 16) (square-list (list 1 2 3 4)))
        "The second #square-list procedure works as expected.")

; {{{2 Exercise 2.22:
; {{{3 Problem
;      Louis Reasoner tries to rewrite the first
;      `square-list' procedure of *Note Exercise 2-21:: so that it
;      evolves an iterative process:
;
;           (define (square-list items)
;             (define (iter things answer)
;               (if (null? things)
;                   answer
;                   (iter (cdr things)
;                         (cons (square (car things))
;                               answer))))
;             (iter items nil))
;
;      Unfortunately, defining `square-list' this way produces the answer
;      list in the reverse order of the one desired.  Why?
;
;      Louis then tries to fix his bug by interchanging the arguments to
;      `cons':
;
;           (define (square-list items)
;             (define (iter things answer)
;               (if (null? things)
;                   answer
;                   (iter (cdr things)
;                         (cons answer
;                               (square (car things))))))
;             (iter items nil))
;
;      This doesn't work either.  Explain.
;
; {{{3 Solution
;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))
(assert (equal? (reverse (list 1 4 9 16)) (square-list (list 1 2 3 4)))
       "The iterative square-list returns a reversed ordered answer:
        The reason for this order is that the first values worked on will be
        the last values of the empty final list.")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil))

(assert (not (equal? (list 1 4 9 16)
                     (square-list (list 1 2 3 4))))
       "The second iteratie square-list procedure does not work either becuase it
        constructs a list of list not a list of numbers.")

; {{{2 Exercise 2.23:
; {{{3 Problem
;      The procedure `for-each' is similar to `map'.  It
;      takes as arguments a procedure and a list of elements.  However,
;      rather than forming a list of the results, `for-each' just applies
;      the procedure to each of the elements in turn, from left to right.
;      The values returned by applying the procedure to the elements are
;      not used at all--`for-each' is used with procedures that perform
;      an action, such as printing.  For example,
;
;           (for-each (lambda (x) (newline) (display x))
;                     (list 57 321 88))
;           57
;           321
;           88
;
;      The value returned by the call to `for-each' (not illustrated
;      above) can be something arbitrary, such as true.  Give an
;      implementation of `for-each'.
;
; {{{3 Solution
(define (for-each f a)
  (if (null? a)
    #t)
  (lambda ()
    (apply f (car a))
    (for-each f (cdr a))))
(assert (for-each (lambda (x) x) (list 1 2 3))
        "#the-foreach procedure works a expected")
; {{{1 2.2.2 Hierarchical Structures
;
; {{{2 Exercise 2.24:
; {{{3 Problem
;      Suppose we evaluate the expression `(list 1 (list
;      2 (list 3 4)))'.  Give the result printed by the interpreter, the
;      corresponding box-and-pointer structure, and the interpretation of
;      this as a tree (as in *Note Figure 2-6::).
(list 1 (list 2 (list 3 4)))

; {{{3 Solution
;
; Box and pointer:
;
;                               (2 (3 4))
;                                   |
;                                   v
;                   _______      _______
; (1 (2 (3 4)))--> | * | *-|--> | * | / |
;                   -|-----      -|-----       (3 4)
;                    |            |              |
;                    v            v              v
;                    1           _______      _______
;                               | * | *-|--> | * | / |
;                                -|-----      -|-----
;                                 |            |
;                                 v            v
;                                 2           _______      _______
;                                            | * | *-|--> | * | / |
;                                             -|-----      -|-----
;                                              |            |
;                                              V            v
;                                              3            4
;
; Tree:
;
;                           (1 (2 (3 4)))
;                                 *
;                                / \
;                               /   \
;                              1     * (2 (3 4))
;                                   / \
;                                  /   \
;                                 2     * (3 4)
;                                      / \
;                                     /   \
;                                    3     4
; {{{2 Exercise 2.25:
; {{{3 Problem
;      Give combinations of `car's and `cdr's that will
;      pick 7 from each of the following lists:
;
;           (1 3 (5 7) 9)
;
;           ((7))
;
;           (1 (2 (3 (4 (5 (6 7))))))
;
; {{{3 Solution

(define (pick-7 a)
  (cadr (caddr a)))

(assert (equal? 7 (pick-7 (list 1 3 (list 5 7) 9))) "Lucky seven!")

(define (pick-7 a)
  (caar a))

(assert (equal? 7 (pick-7 (list (list 7)))) "Lucky seven!")

(define (pick-7 a)
  (cadadr (cadadr (cadadr a))))

(assert (equal? 7 (pick-7 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))) "Lucky seven!")

; {{{2 Exercise 2.26:
; {{{3 Problem
;      Suppose we define `x' and `y' to be two lists:
;
;           (define x (list 1 2 3))
;
;           (define y (list 4 5 6))
;
;      What result is printed by the interpreter in response to
;      evaluating each of the following expressions:
;
;           (append x y)
;
;           (cons x y)
;
;           (list x y)
;
; {{{3 Solution
(define x (list 1 2 3))
(define y (list 4 5 6))
(assert (equal? (list 1 2 3 4 5 6) (append x y))
        "(append x y) results in (1 2 3 4 5 6).")

(assert (equal? (list (list 1 2 3) 4 5 6) (cons x y))
        "(cons x y) results in ((1 2 3) 4 5 6).")

(assert (equal? (list (list 1 2 3) (list 4 5 6)) (list x y))
        "(list x y) results in ((1 2 3) (4 5 6).")
; {{{2 Exercise 2.27:
; {{{3 Problem
;      Modify your `reverse' procedure of *Note Exercise
;      2-18:: to produce a `deep-reverse' procedure that takes a list as
;      argument and returns as its value the list with its elements
;      reversed and with all sublists deep-reversed as well.  For example,
;
;           (define x (list (list 1 2) (list 3 4)))
;
;           x
;           ((1 2) (3 4))
;
;           (reverse x)
;           ((3 4) (1 2))
;
;           (deep-reverse x)
;           ((4 3) (2 1))
;
; {{{3 Solution
(define (deep-reverse a-list)
  (if (null? a-list)
    a-list
    (append (deep-reverse (cdr a-list))
            (if (pair? (car a-list))
              (list (deep-reverse (car a-list)) )
              (list (car a-list))))))

(define x (list (list 1 2) (list 3 4)))
(assert (equal? (list (list 4 3) (list 2 1))
                (deep-reverse x))
        "#deep-reverse works as expected.")
; {{{2 Exercise 2.28:
; {{{3 Problem
;      Write a procedure `fringe' that takes as argument
;      a tree (represented as a list) and returns a list whose elements
;      are all the leaves of the tree arranged in left-to-right order.
;      For example,
;
;           (define x (list (list 1 2) (list 3 4)))
;
;           (fringe x)
;           (1 2 3 4)
;
;           (fringe (list x x))
;           (1 2 3 4 1 2 3 4)
;
; {{{3 Solution
(define (fringe a-list)
  (if (null? a-list)
    a-list
    (append (if (pair? (car a-list))
              (fringe (car a-list))
              (list (car a-list)))
            (fringe (cdr a-list)))))

(define x (list (list 1 2) (list 3 4 (list 5 6))))
(assert (equal? (list 1 2 3 4 5 6) (fringe x))
        "(fringe ((1 2) (3 4 (5 6)) equals (1 2 3 4 5 6)")

(assert (equal? (list 1 2 3 4 5 6 1 2 3 4 5 6)
                (fringe (list x x)))
        "(fringe (list x x)) equals (1 2 3 4 5 6 1 2 3 4 5 6)")

; {{{2 Exercise 2.29:
; {{{3 Problem
;      A binary mobile consists of two branches, a left
;      branch and a right branch.  Each branch is a rod of a certain
;      length, from which hangs either a weight or another binary mobile.
;      We can represent a binary mobile using compound data by
;      constructing it from two branches (for example, using `list'):
;
           (define (make-mobile left right)
             (list left right))
;
;      A branch is constructed from a `length' (which must be a number)
;      together with a `structure', which may be either a number
;      (representing a simple weight) or another mobile:
;
           (define (make-branch length structure)
             (list length structure))
;
;        a. Write the corresponding selectors `left-branch' and
;           `right-branch', which return the branches of a mobile, and
;           `branch-length' and `branch-structure', which return the
;           components of a branch.
;
;        b. Using your selectors, define a procedure `total-weight' that
;           returns the total weight of a mobile.
;
;        c. A mobile is said to be "balanced" if the torque applied by
;           its top-left branch is equal to that applied by its top-right
;           branch (that is, if the length of the left rod multiplied by
;           the weight hanging from that rod is equal to the
;           corresponding product for the right side) and if each of the
;           submobiles hanging off its branches is balanced. Design a
;           predicate that tests whether a binary mobile is balanced.
;
;        d. Suppose we change the representation of mobiles so that the
;           constructors are
;
;                (define (make-mobile left right)
;                  (cons left right))
;
;                (define (make-branch length structure)
;                  (cons length structure))
;
;           How much do you need to change your programs to convert to
;           the new representation?
;
; {{{3 Solution
(define (left-branch mobile)
  (car mobile))

(define v (make-branch 2 2))
(define w (make-branch 1 1))
(define x (make-branch 1 2))

(define a (make-mobile w x))

(define y (make-branch 1 a))
(define z (make-branch 1 4))

(define b (make-mobile y z))
(define c (make-mobile v z))


(assert (equal? w (left-branch a))
        "(left-branch a) equals w")

(define (right-branch mobile)
  (cadr mobile))

(assert (equal? x (right-branch a))
        "(right-branch a) equals x")

(define (total-sum a-list)
  (if (null? a-list)
    0
    (+ (car a-list) (total-sum (cdr a-list)))))

(assert (= 10 (total-sum (list 1 2 3 4)))
        "(total-sum (1 2 3 4)) equals 10.")

(define (weight branch)
  (if (pair? (cadr branch))
    (total-weight (cadr branch))
    (cadr branch)))

(define (total-weight mobile)
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(assert (= 7 (total-weight b))
        "(total-weight a b) is 7")

(define (torque branch)
  (define (length branch)
    (car branch))
  (* (weight branch) (length branch)))

(assert (= 6 (torque (make-branch 2 3)))
        "(torque (make-branch 2 3)) is 6")
(torque (left-branch b))

(define (balanced? mobile)
  (define (sub-balanced? branch)
    (if (pair? (cadr branch))
      (balanced? (cadr branch))
      #t))
  (and
    (= (torque (left-branch mobile))
       (torque (right-branch mobile)))
    (sub-balanced? (left-branch mobile))
    (sub-balanced? (right-branch mobile))))

(assert (not (balanced? a))
        "mobile a is not balanced")

(assert (balanced? c)
        "mobile c is not balanced")

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define v (make-branch 2 2))
(define w (make-branch 1 1))
(define x (make-branch 1 2))

(define a (make-mobile w x))

(define y (make-branch 1 a))
(define z (make-branch 1 4))

(define b (make-mobile y z))
(define c (make-mobile v z))

(assert (equal? w (left-branch a))
        "(left-branch a) equals w")

(define (right-branch mobile)
  (cdr mobile))

(assert (equal? x (right-branch a))
        "(right-branch a) equals x")

(define (weight branch)
  (if (pair? (cdr branch))
    (total-weight (cdr branch))
    (cdr branch)))

(assert (= 7 (total-weight b))
        "(total-weight a b) is 7")

(define (balanced? mobile)
  (define (sub-balanced? branch)
    (if (pair? (cdr branch))
      (balanced? (cdr branch))
      #t))
  (and
    (= (torque (left-branch mobile))
       (torque (right-branch mobile)))
    (sub-balanced? (left-branch mobile))
    (sub-balanced? (right-branch mobile))))

(assert (not (balanced? a))
        "mobile a is not balanced")

(assert (balanced? c)
        "mobile c is not balanced")

; {{{2 Exercise 2.30:
; {{{3 Problem
;      Define a procedure `square-tree' analogous to the
;      `square-list' procedure of *Note Exercise 2-21::.  That is,
;      `square-list' should behave as follows:
;
;           (square-tree
;            (list 1
;                  (list 2 (list 3 4) 5)
;                  (list 6 7)))
;           (1 (4 (9 16) 25) (36 49))
;
;      Define `square-tree' both directly (i.e., without using any
;      higher-order procedures) and also by using `map' and recursion.
;
; {{{3 Solution
(define (square-tree a-tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       a-tree))

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define y (list 1 (list 4 (list 9 16) 25) (list 36 49)))
(assert (equal? y (square-tree x))
        "(square-tree (1 (2 ( 3 4) 5 (6 7)))) equals (1 (4 (9 16) 25 (36 42)")
; {{{2 Exercise 2.31:
; {{{3 Problem
;      Abstract your answer to *Note Exercise 2-30:: to
;      produce a procedure `tree-map` with the property that
;      `square-tree` could be defined as
;
;           (define (square-tree tree) (tree-map square tree))
;
; {{{3 Solution
(define (tree-map f a-tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       a-tree))

(define (square-tree a-tree)
  (tree-map square a-tree))

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define y (list 1 (list 4 (list 9 16) 25) (list 36 49)))
(assert (equal? y (square-tree x))
        "The refactored #tree-map works as expected.")

; {{{2 Exercise 2.32:
; {{{3 Problem
;      We can represent a set as a list of distinct
;      elements, and we can represent the set of all subsets of the set as
;      a list of lists.  For example, if the set is `(1 2 3)', then the
;      set of all subsets is `(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2
;      3))'.  Complete the following definition of a procedure that
;      generates the set of subsets of a set and give a clear explanation
;      of why it works:
;
;           (define (subsets s)
;             (if (null? s)
;                 (list nil)
;                 (let ((rest (subsets (cdr s))))
;                   (append rest (map <??> rest)))))
;
; {{{3 Solution
;

(define (subsets s)
  (define (subset item)
    (cons (car s) item))
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map subset rest)))))

(define (subsets s)
  (define (list-filter terms)
    (lambda (item)
      (define (in-list? terms)
        (if (null? terms)
          #t
          (if (= (car terms) item)
            #f
            (or (in-list? (cdr terms))))))
      (in-list? terms)))
  (define (filter-subset terms)
    (filter (list-filter terms) s))
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map filter-subset rest)))))

; Result without using a filter:
;
; (1 2 3)
; (2 3)
; (3)
; ()
; term - (), subset - (3)
; term - (), subset - (2 3)
; term - (3), subset - (2 3)
; term - (), subset - (1 2 3)
; term - (3), subset - (1 2 3)
; term - (2 3), subset - (1 2 3)
; term - (2 3), subset - (1 2 3)
;
; (() (3) (2 3) (2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3))
;
; Result with filter:
;
; (1 2 3)
; (2 3)
; (3)
; ()
;
; terms - (), subset - (3)
; terms - (), subset - (2 3)
; terms - (3), subset - (2 3)
; terms - (), subset - (1 2 3)
; terms - (3), subset - (1 2 3)
; terms - (2 3), subset - (1 2 3)
; terms - (2), subset - (1 2 3)
;
; (() (3) (2 3) (2) (1 2 3) (1 2) (1) (1 3))

(define x (list 1 2 3))
(define y (list nil  (list 3) (list 2 3) (list 2) (list 1 2 3) (list 1 2) (list 1) (list 1 3)))
(assert (equal? y (subsets x))
        "(subsets (1 2 3)) equals (() (3) (2 3) (2) (1 2 3) (1 2) (1) (1 3))")

; {{{1 2.2.3 Sequences as Conventional Interfaces
;
; {{{2 Exercise 2.33:
; {{{3 Problem
;      Fill in the missing expressions to complete the
;      following definitions of some basic list-manipulation operations
;      as accumulations:
;
;           (define (map p sequence)
;             (accumulate (lambda (x y) <??>) nil sequence))
;
;           (define (append seq1 seq2)
;             (accumulate cons <??> <??>))
;
;           (define (length sequence)
;             (accumulate <??> 0 sequence))
;
; {{{3 Solution
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; (define (map p sequence)
;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence)

(define x (list 1 2 3 4))
(define y (list 1 4 9 16))
(assert (equal? y (map square x))
        "(map square (1 2 3 4)) equals (1 4 9 16)")

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define x (list 1 2 3))
(define y (list 4 5 6))
(define z (list 1 2 3 4 5 6))
(assert (equal? z (append x y))
        "(append (1 2 3) (4 5 6)) equals (1 2 3 4 5 6)")

(define (length sequence)
  (define (unity thunk) 1)
  (accumulate +  0 (map unity sequence)))

(define x (list 1 2 3 4 5))
(assert (= 5 (length x))
        "(length (1 2 3 4 5)) equals 5")

; {{{2 Exercise 2.34:
; {{{3 Problem
;      Evaluating a polynomial in x at a given value of
;      x can be formulated as an accumulation.  We evaluate the polynomial
;
;           a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0
;
;      using a well-known algorithm called "Horner's rule", which
;      structures the computation as
;
;           (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0
;
;      In other words, we start with a_n, multiply by x, add a_(n-1),
;      multiply by x, and so on, until we reach a_0.(3)
;
;      Fill in the following template to produce a procedure that
;      evaluates a polynomial using Horner's rule.  Assume that the
;      coefficients of the polynomial are arranged in a sequence, from
;      a_0 through a_n.
;
;           (define (horner-eval x coefficient-sequence)
;             (accumulate (lambda (this-coeff higher-terms) <??>)
;                         0
;                         coefficient-sequence))
;
;      For example, to compute 1 + 3x + 5x^3 + x^(5) at x = 2 you would
;      evaluate
;
;           (horner-eval 2 (list 1 3 0 5 0 1))
;
; {{{3 Solution
;
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+  this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define x (list 1 3 0 5 0 1))
(define y (+ 1 (* 2 (+ 3 (* 2 (+ 0 (* 2 (+ 5 (* 2 (+ 0 (* 2 (+ 1 0))))))))))))
(assert (= 79 (horner-eval 2 x))
        "(horner-eval (1 3 0 5 0 1)) equals 79.")

; {{{2 Exercise 2.35:
; {{{3 Problem
;      Redefine `count-leaves' from section *Note
;      2-2-2:: as an accumulation:
;
;           (define (count-leaves t)
;             (accumulate <??> <??> (map <??> <??>)))
;
; {{{3 Solution
;
(define (count-leaves t)
  (accumulate + 0 (map (lambda(x)
                         (if (pair? x)
                           (count-leaves x)
                           1))
                       t)))

(define x (list 1 (list (list (list 2))) 3 (list (list (list 4)))))
(assert (= 4 (count-leaves x))
        "(count-leaves (1 ((2)) 3 (((4)))) equals 4")

(define x (list 1 (list 2 3 (list 4))))
(assert (= 4  (count-leaves x))
        "(count-leaves (1 ((2 3 (((4)))))) equals 4")

; {{{2 Exercise 2.36:
; {{{3 Problem
;      The procedure `accumulate-n' is similar to
;      `accumulate' except that it takes as its third argument a sequence
;      of sequences, which are all assumed to have the same number of
;      elements.  It applies the designated accumulation procedure to
;      combine all the first elements of the sequences, all the second
;      elements of the sequences, and so on, and returns a sequence of
;      the results.  For instance, if `s' is a sequence containing four
;      sequences, `((1 2 3) (4 5 6) (7 8 9) (10 11 12)),' then the value
;      of `(accumulate-n + 0 s)' should be the sequence `(22 26 30)'.
;      Fill in the missing expressions in the following definition of
;      `accumulate-n':
;
;           (define (accumulate-n op init seqs)
;             (if (null? (car seqs))
;                 nil
;                 (cons (accumulate op init <??>)
;                       (accumulate-n op init <??>))))
;

; {{{3 Solution
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map (lambda (x)
                                     (car x))
                                   seqs))
          (accumulate-n op init (map (lambda (x)
                                       (cdr x))
                                     seqs)))))

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define y (list 22 26 30))
(assert (equal? y (accumulate-n + 0 x))
        "(accumulate-n + 0 ((1 2 3) (4 5 6) (7 8 9) (10 11 12))) equals (22 26 30)")

; {{{2 Exercise 2.37
;
; Suppose we represent vectors v = (v_i) as sequences of numbers, and
; matrices m = (m_(ij)) as sequences of vectors (the rows of the matrix).
; For example, the matrix
;
;      +-         -+
;      |  1 2 3 4  |
;      |  4 5 6 6  |
;      |  6 7 8 9  |
;      +-         -+
;
; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.  With
; this representation, we can use sequence operations to concisely
; express the basic matrix and vector operations.  These operations
; (which are described in any book on matrix algebra) are the following:
;
;                                             __
;      (dot-product v w)      returns the sum >_i v_i w_i
;
;      (matrix-*-vector m v)  returns the vector t,
;                                         __
;                             where t_i = >_j m_(ij) v_j
;
;      (matrix-*-matrix m n)  returns the matrix p,
;                                            __
;                             where p_(ij) = >_k m_(ik) n_(kj)
;
;      (transpose m)          returns the matrix n,
;                             where n_(ij) = m_(ji)
;
;    We can define the dot product as(4)
;
;      (define (dot-product v w)
;        (accumulate + 0 (map * v w)))
;
;    Fill in the missing expressions in the following procedures for
; computing the other matrix operations.  (The procedure `accumulate-n'
; is defined in *Note Exercise 2-36::.)
;
;      (define (matrix-*-vector m v)
;        (map <??> m))
;
;      (define (transpose mat)
;        (accumulate-n <??> <??> mat))
;
;      (define (matrix-*-matrix m n)
;        (let ((cols (transpose n)))
;          (map <??> m)))
;
; {{{3 Solution
; {{{2 Exercise 2.38:
; {{{3 Problem
;      The `accumulate' procedure is also known as
;      `fold-right', because it combines the first element of the
;      sequence with the result of combining all the elements to the
;      right.  There is also a `fold-left', which is similar to
;      `fold-right', except that it combines elements working in the
;      opposite direction:
;
;           (define (fold-left op initial sequence)
;             (define (iter result rest)
;               (if (null? rest)
;                   result
;                   (iter (op result (car rest))
;                         (cdr rest))))
;             (iter initial sequence))
;
;      What are the values of
;
;           (fold-right / 1 (list 1 2 3))
;
;           (fold-left / 1 (list 1 2 3))
;
;           (fold-right list nil (list 1 2 3))
;
;           (fold-left list nil (list 1 2 3))
;
;      Give a property that `op' should satisfy to guarantee that
;      `fold-right' and `fold-left' will produce the same values for any
;      sequence.
;
; {{{3 Solution
; {{{2 Exercise 2.39:
; {{{3 Problem
;      Complete the following definitions of `reverse'
;      (*Note Exercise 2-18::) in terms of `fold-right' and `fold-left'
;      from *Note Exercise 2-38:::
;
;           (define (reverse sequence)
;             (fold-right (lambda (x y) <??>) nil sequence))
;
;           (define (reverse sequence)
;             (fold-left (lambda (x y) <??>) nil sequence))
;
; {{{3 Solution
; {{{2 Exercise 2.40:
; {{{3 Problem
;      Define a procedure `unique-pairs' that, given an
;      integer n, generates the sequence of pairs (i,j) with 1 <= j< i <=
;      n.  Use `unique-pairs' to simplify the definition of
;      `prime-sum-pairs' given above.
;
; {{{3 Solution
; {{{2 Exercise 2.41:
; {{{3 Problem
;      Write a procedure to find all ordered triples of
;      distinct positive integers i, j, and k less than or equal to a
;      given integer n that sum to a given integer s.
;
;      *Figure 2.8:* A solution to the eight-queens puzzle.
;
;           +---+---+---+---+---+---+---+---+
;           |   |   |   |   |   | Q |   |   |
;           +---+---+---+---+---+---+---+---+
;           |   |   | Q |   |   |   |   |   |
;           +---+---+---+---+---+---+---+---+
;           | Q |   |   |   |   |   |   |   |
;           +---+---+---+---+---+---+---+---+
;           |   |   |   |   |   |   | Q |   |
;           +---+---+---+---+---+---+---+---+
;           |   |   |   |   | Q |   |   |   |
;           +---+---+---+---+---+---+---+---+
;           |   |   |   |   |   |   |   | Q |
;           +---+---+---+---+---+---+---+---+
;           |   | Q |   |   |   |   |   |   |
;           +---+---+---+---+---+---+---+---+
;           |   |   |   | Q |   |   |   |   |
;           +---+---+---+---+---+---+---+---+
;
; {{{3 Solution
; {{{2 Exercise 2.42:
; {{{3 Problem
;      The "eight-queens puzzle" asks how to place eight
;      queens on a chessboard so that no queen is in check from any other
;      (i.e., no two queens are in the same row, column, or diagonal).
;      One possible solution is shown in *Note Figure 2-8::.  One way to
;      solve the puzzle is to work across the board, placing a queen in
;      each column.  Once we have placed k - 1 queens, we must place the
;      kth queen in a position where it does not check any of the queens
;      already on the board.  We can formulate this approach recursively:
;      Assume that we have already generated the sequence of all possible
;      ways to place k - 1 queens in the first k - 1 columns of the
;      board.  For each of these ways, generate an extended set of
;      positions by placing a queen in each row of the kth column.  Now
;      filter these, keeping only the positions for which the queen in
;      the kth column is safe with respect to the other queens.  This
;      produces the sequence of all ways to place k queens in the first k
;      columns.  By continuing this process, we will produce not only one
;      solution, but all solutions to the puzzle.
;
;      We implement this solution as a procedure `queens', which returns a
;      sequence of all solutions to the problem of placing n queens on an
;      n*n chessboard.  `Queens' has an internal procedure `queen-cols'
;      that returns the sequence of all ways to place queens in the first
;      k columns of the board.
;
;           (define (queens board-size)
;             (define (queen-cols k)
;               (if (= k 0)
;                   (list empty-board)
;                   (filter
;                    (lambda (positions) (safe? k positions))
;                    (flatmap
;                     (lambda (rest-of-queens)
;                       (map (lambda (new-row)
;                              (adjoin-position new-row k rest-of-queens))
;                            (enumerate-interval 1 board-size)))
;                     (queen-cols (- k 1))))))
;             (queen-cols board-size))
;
;      In this procedure `rest-of-queens' is a way to place k - 1 queens
;      in the first k - 1 columns, and `new-row' is a proposed row in
;      which to place the queen for the kth column.  Complete the program
;      by implementing the representation for sets of board positions,
;      including the procedure `adjoin-position', which adjoins a new
;      row-column position to a set of positions, and `empty-board',
;      which represents an empty set of positions.  You must also write
;      the procedure `safe?', which determines for a set of positions,
;      whether the queen in the kth column is safe with respect to the
;      others.  (Note that we need only check whether the new queen is
;      safe--the other queens are already guaranteed safe with respect to
;      each other.)
;
; {{{3 Solution
; {{{2 Exercise 2.43:
; {{{3 Problem
;      Louis Reasoner is having a terrible time doing
;      *Note Exercise 2-42::.  His `queens' procedure seems to work, but
;      it runs extremely slowly.  (Louis never does manage to wait long
;      enough for it to solve even the 6*6 case.)  When Louis asks Eva Lu
;      Ator for help, she points out that he has interchanged the order
;      of the nested mappings in the `flatmap', writing it as
;
;           (flatmap
;            (lambda (new-row)
;              (map (lambda (rest-of-queens)
;                     (adjoin-position new-row k rest-of-queens))
;                   (queen-cols (- k 1))))
;            (enumerate-interval 1 board-size))
;
;      Explain why this interchange makes the program run slowly.
;      Estimate how long it will take Louis's program to solve the
;      eight-queens puzzle, assuming that the program in *Note Exercise
;      2-42:: solves the puzzle in time T.
;
; {{{3 Solution
;
; {{{1 2.2.4 Example: A Picture Language
;
; {{{2 Exercise 2.44:
; {{{3 Problem
;      Define the procedure `up-split' used by
;      `corner-split'.  It is similar to `right-split', except that it
;      switches the roles of `below' and `beside'.
;
; {{{3 Solution
; {{{2 Exercise 2.45:
; {{{3 Problem
;      `Right-split' and `up-split' can be expressed as
;      instances of a general splitting operation.  Define a procedure
;      `split' with the property that evaluating
;
;           (define right-split (split beside below))
;           (define up-split (split below beside))
;
;      produces procedures `right-split' and `up-split' with the same
;      behaviors as the ones already defined.
;
; {{{3 Solution
; {{{2 Exercise 2.46:
; {{{3 Problem
;      A two-dimensional vector v running from the
;      origin to a point can be represented as a pair consisting of an
;      x-coordinate and a y-coordinate.  Implement a data abstraction for
;      vectors by giving a constructor `make-vect' and corresponding
;      selectors `xcor-vect' and `ycor-vect'.  In terms of your selectors
;      and constructor, implement procedures `add-vect', `sub-vect', and
;      `scale-vect' that perform the operations vector addition, vector
;      subtraction, and multiplying a vector by a scalar:
;
;           (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;           (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;                        s * (x, y) = (sx, sy)
;
; {{{3 Solution
; {{{2 Exercise 2.47:
; {{{3 Problem
;      Here are two possible constructors for frames:
;
;           (define (make-frame origin edge1 edge2)
;             (list origin edge1 edge2))
;
;           (define (make-frame origin edge1 edge2)
;             (cons origin (cons edge1 edge2)))
;
;      For each constructor supply the appropriate selectors to produce an
;      implementation for frames.
;
; {{{3 Solution
; {{{2 Exercise 2.48:
; {{{3 Problem
;      A directed line segment in the plane can be
;      represented as a pair of vectors--the vector running from the
;      origin to the start-point of the segment, and the vector running
;      from the origin to the end-point of the segment.  Use your vector
;      representation from *Note Exercise 2-46:: to define a
;      representation for segments with a constructor `make-segment' and
;      selectors `start-segment' and `end-segment'.
;
; {{{3 Solution
; {{{2 Exercise 2.49:
; {{{3 Problem
;      Use `segments->painter' to define the following
;      primitive painters:
;
;        a. The painter that draws the outline of the designated frame.
;
;        b. The painter that draws an "X" by connecting opposite corners
;           of the frame.
;
;        c. The painter that draws a diamond shape by connecting the
;           midpoints of the sides of the frame.
;
;        d. The `wave' painter.
;
; {{{3 Solution
; {{{2 Exercise 2.50:
; {{{3 Problem
;      Define the transformation `flip-horiz', which
;      flips painters horizontally, and transformations that rotate
;      painters counterclockwise by 180 degrees and 270 degrees.
;
; {{{3 Solution
; {{{2 Exercise 2.51:
; {{{3 Problem
;      Define the `below' operation for painters.
;      `Below' takes two painters as arguments.  The resulting painter,
;      given a frame, draws with the first painter in the bottom of the
;      frame and with the second painter in the top.  Define `below' in
;      two different ways--first by writing a procedure that is analogous
;      to the `beside' procedure given above, and again in terms of
;      `beside' and suitable rotation operations (from *Note Exercise
;      2-50::).
;
; {{{3 Solution
; {{{2 Exercise 2.52:
; {{{3 Problem
;      Make changes to the square limit of `wave' shown
;      in *Note Figure 2-9:: by working at each of the levels described
;      above.  In particular:
;
;        a. Add some segments to the primitive `wave' painter of *Note
;           Exercise 2-49:: (to add a smile, for example).
;
;        b. Change the pattern constructed by `corner-split' (for
;           example, by using only one copy of the `up-split' and
;           `right-split' images instead of two).
;
;        c. Modify the version of `square-limit' that uses
;           `square-of-four' so as to assemble the corners in a different
;           pattern.  (For example, you might make the big Mr. Rogers
;           look outward from each corner of the square.)
;
; {{{3 Solution
;
