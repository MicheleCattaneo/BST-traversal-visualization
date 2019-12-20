;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Project by Michele Cattaneo and Davide Scannapieco
;---------------------------------------------------------------------------
;
;
;                         TREE TRAVERSAL VISUALIZER
;
;
;---------------------------------------------------------------------------
; Goal: This software allows to interactively visualize a traversal algoroithm on
; one or more given Binary Search Trees.
;
; The following algorithms can be seen:
;  - In Order Traversal
;  - Pre Order Traversal
;  - Post Order Traversal
;
; To try your own BSTs:
; Modify the multiple.csv file as follows:
; Each row of comma separated numbers represents a tree
; This row generates a tree going backwards, meaning that the last number of the list will be the
; the root of the whole tree, the second-last number will be inserted in the tree made of the last
; one ( a tree of 1 node ), the third-last will be inserted in the tree made by the previous 2 and so on
; For simplicity, given a tree, to visualize it, it is required to write the numbers starting from the
; lowest level, from left to right, going up to the next level up until the root
; Example:
;          20
;        /   \
;      10    30
;     /  \     \
;    5   15     35
; Is the result of a list formed like this: 5, 15, 35, 10, 30, 20
;
; To run the code:
; type: (main 0) inside the interaction area

; GLOBAL CONSTANTS
; Heigth and width of the graphical interface
(define HEIGTH 500)
(define WIDTH 1500)

; Libraries Requiring
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

; a Node is one of :
; (make-leaf)
; (make-node BST Value BST x y Color)
; a value is a number, representing the value of the node
; x y are coordinates of the node given to represent it on a canvas
; a color is a number and represents one of the possible coloration that a node has
; NodeColor is one of:
; - 0 for white
; - 1 for red
; - 2 for green

; an App is a (make-app tree iterator alg current-tree depth event-list radius ) where:
; tree is a BST representing the tree currently being displayed
; iterator is a Number representing the number of steps already performed in the algorithm
; alg is a Number representing one of the possible algorithms to perform
; current-tree is a Number representing which list of the List<List<Number>> is currently used as tree
; depth is a Number representing the depth of each level of the BST when displayed
; event-list is a List<Number> representing a list of successive Node values being visited by the algorithm
; radius is a Number representing the radius of each node when displayed

(define-struct node [left value right x y color])
(define-struct leaf [])
(define-struct app [tree iterator alg current-tree depth event-list radius])

;---------------------------------------------------------------------------
;
;
;                               NOT-SET-NODE
;
;
;---------------------------------------------------------------------------

; BST  Number  BST -> Node
; Given a number and two BST, it returns a node where x y and color are not set yet.

(define (not-set-node tree1 x tree2)
  (make-node tree1 x tree2 0 0 0))

(check-expect (not-set-node (make-leaf) 0 (make-leaf)) (make-node (make-leaf) 0 (make-leaf) 0 0 0))

;---------------------------------------------------------------------------
;
;
;                                 INSERT-TREE
;
;
;---------------------------------------------------------------------------

; Number BST -> BST
; Given a number, return a BST (Binary Search Tree) with the new number inserted

(define (insert-tree x tree)
  (cond [(leaf? tree) (not-set-node (make-leaf) x (make-leaf))]
        [(node? tree) (cond [(< x (node-value tree)) (not-set-node (insert-tree x (node-left tree))
                                                                   (node-value tree)
                                                                   (node-right tree))]
                            [( = x (node-value tree)) tree] 
                            [(> x (node-value tree)) (not-set-node (node-left tree)
                                                                   (node-value tree)
                                                                   (insert-tree x (node-right tree)))])]))

(define SET-1
  (not-set-node(not-set-node (not-set-node(make-leaf) 2 (make-leaf))
                            3
                            (not-set-node(make-leaf) 4 (make-leaf)))
               6
               (not-set-node(not-set-node(make-leaf) 7 (make-leaf))
                            8
                            (not-set-node(make-leaf) 9 (make-leaf)))))

(check-expect (insert-tree 4 (make-leaf)) (not-set-node (make-leaf) 4 (make-leaf)))

(check-expect (insert-tree 10 SET-1) (not-set-node(not-set-node(not-set-node(make-leaf) 2 (make-leaf))
                            3
                            (not-set-node(make-leaf) 4 (make-leaf)))
               6
               (not-set-node(not-set-node(make-leaf) 7 (make-leaf))
                            8
                            (not-set-node(make-leaf) 9 (not-set-node (make-leaf) 10 (make-leaf))))))

;---------------------------------------------------------------------------
;
;
;                              BST-NODE 
;
;
;---------------------------------------------------------------------------

; List<Number> -> BST 
; Given a list of number, returns a new BST with that list of number as nodes.

(define (list-insert-tree xs)
  (cond[(empty? xs) (make-leaf)]
       [else (insert-tree (first xs) (list-insert-tree (rest xs)))]))
               


(check-expect (list-insert-tree (list 3 2 1))(make-node (make-leaf)
           1
           (make-node
            (make-leaf)
            2
            (make-node
             (make-leaf)
             3
             (make-leaf) 0 0 0) 0 0 0) 0 0 0))
(check-expect (list-insert-tree (list 11 4 16 6 15 5 10))
              (make-node
               (make-node (make-node (make-leaf) 4 (make-leaf) 0 0 0) 5
                          (make-node (make-leaf) 6 (make-leaf) 0 0 0) 0 0 0)
               10 (make-node (make-node (make-leaf)
                                        11 (make-leaf) 0 0 0)
                             15 (make-node (make-leaf) 16 (make-leaf) 0 0 0) 0 0 0) 0 0 0))

;---------------------------------------------------------------------------
;
;
;                              SAVE-NODES 
;
;
;---------------------------------------------------------------------------

; BST -> List<List<Number>>
; Given a BST with the coordinates set, returns a list of list  containing all coordinates of the nodes and all
; parameters.
(define (save-nodes tree)
  (cond [(leaf? tree) '()]
        [else (append
               (list(list (node-x tree) (node-y tree) (node-value tree) (node-color tree)))
               (save-nodes (node-left tree))
               (save-nodes (node-right tree)))])) 


(check-expect (save-nodes SET-1) (list (list 0 0 6 0) (list 0 0 3 0) (list 0 0 2 0)
                                       (list 0 0 4 0) (list 0 0 8 0) (list 0 0 7 0) (list 0 0 9 0)))

;---------------------------------------------------------------------------
;
;
;                             MAKE-IN-ORDER-EVENTS
;
;
;---------------------------------------------------------------------------

; BST -> List<Number>
; Given a Tree returns a list such that is the combination of a pre-order and in-order traversal
; this way a number is inserted into the list twice, the first time when visited and the second time
; when marked by the algorithm we want.
; Later, when the color needs to be set, say we are at the i-th element of the list, if in the sublist
; going from 0 to i-1 such node is already present, that node at index i will be coloured in red, otherwise
; in green.

(define (make-in-order-events tree)
  (cond [(leaf? tree) ' ()]
        [else (append (list(node-value tree))
                      (make-in-order-events (node-left tree))
                      (list(node-value tree))
                      (make-in-order-events (node-right tree))
                      )]))


(check-expect (make-in-order-events SET-1) 
              (list 6 3 2 2 3 4 4 6 8 7 7 8 9 9))
;---------------------------------------------------------------------------
;
;
;                             MAKE-POST-ORDER-EVENTS
;
;
;---------------------------------------------------------------------------

; BST -> List<Number>
; Same concept as the make-in-order-events function, the only difference is the combination
; of pre-order and post-order

(define (make-post-order-events tree)
  (cond [(leaf? tree) ' ()]
        [else (append (list(node-value tree))
                      (make-post-order-events (node-left tree))
                      (make-post-order-events (node-right tree))
                      (list(node-value tree)))]))

(check-expect (make-post-order-events SET-1) 
              (list 6 3 2 2 4 4 3 8 7 7 9 9 8 6))

;---------------------------------------------------------------------------
;
;
;                             MAKE-PRE-ORDER-EVENTS
;
;
;---------------------------------------------------------------------------

; BST -> List<Number>
; Same concept as the make-in-order-events function, the only difference is the combination
; of two pre-orders

(define (make-pre-order-events tree)
  (cond [(leaf? tree) '()]
        [else (append (list(node-value tree))
                      (list(node-value tree))
                      (make-pre-order-events (node-left tree))
                      (make-pre-order-events (node-right tree)))]))

(check-expect (make-pre-order-events SET-1) 
              (list 6 6 3 3 2 2 4 4 8 8 7 7 9 9 ))
                      
;---------------------------------------------------------------------------
;
;
;                              CONTAINS? 
;
;
;---------------------------------------------------------------------------

; Number List<Number> -> Boolean
; Given a number and a list of numbers, returns a boolean,
; true if the number is present, false if not

(define (contains? x xs)
  (cond [(empty? xs) #false]
        [(= (first xs) x) #true]
        [else (contains? x (rest xs))]))

(check-expect (contains? 5 (list 4 5 6 7 8 )) #true)
(check-expect (contains? 0 (list 4 5 6 7 8 )) #false)

;--------------------------------------------------------------------------
;
;
;                              TAKE
;
;
;---------------------------------------------------------------------------


; Number List<Number> -> List<Number>
; Given a number and a list, return the first n elements of the list

(define (take n xs)
  (cond [(zero? n) '()]
        [(< n 0) '()]
        [(empty? xs) '()]
        [else (cons (first xs) (take (sub1 n) (rest xs)))]))


(check-expect (take 1 (list 1 2 3 4)) (list 1))
(check-expect (take 4 (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (take 0 (list 1 2 3 4)) empty)

;--------------------------------------------------------------------------
;
;
;                              DROP
;
;
;---------------------------------------------------------------------------

; Number List<Number> -> List<Number>
; Given a number and a list, return the list with n elements less

(define (drop n xs)
  (cond [(zero? n) xs]
        [(empty? xs) '()]
        [else (drop (sub1 n) (rest xs))]))

(check-expect (drop 5 (list 1 2 3 4)) ' ())
(check-expect (drop 1 (list 1 2 3 4)) (list 2 3 4))

;--------------------------------------------------------------------------
;
;
;                              SET-COLOR
;
;
;---------------------------------------------------------------------------

; Number List<Number> -> posn
; Given a number a list<Number> Depending on whether a number=xs(i) is present in the list xs(0)-xs(i-1)
; returns a node with the corrisponding color

(define (set-color i xs)
  (set-color-helper i (drop i xs) xs))

;--------------------------------------------------------------------------
;
;
;                              SET-COLOR-HELPER
;
;
;---------------------------------------------------------------------------

; Number List<Number> List<Number> -> posn
; Helper function for the one above, such that the list with dropped elements
; is passed as a paramenter instead of being compute multiple times

(define (set-color-helper i dropped xs) 
  (cond [(contains? (first dropped) (take  i xs)) (make-posn (first dropped) 2)]
        [else (make-posn (first dropped) 1)]))

(check-expect (set-color-helper 0 (drop 0 (list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))(list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))
              (make-posn 6 1))
(check-expect (set-color-helper 2 (drop 2 (list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))(list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))
              (make-posn 2 1))
(check-expect (set-color-helper 3 (drop 2 (list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))(list 6 3 2 2 2 3 4 4 4 3 6 8 7 7 7 8 9 9 9 8 6))
              (make-posn 2 2))
  
;--------------------------------------------------------------------------
;
;
;                              SAVE-LINES
;
;
;---------------------------------------------------------------------------  

;Tree -> List<posn<posn>>
;Given a Tree with the coordinates set, returns a list of posn, where each posn contains two other posn
;the first one contains the tuple of x,y representing the source coordinates of an edge (line)
;the second one contains the tuple of x,x representing the destingation coordinates of an edge (line)

(define (save-line tree)
  (cond
    [(leaf? tree) ' ()]
    [else(append
      (cond
        [(node? (node-left tree))(list (make-posn
                                     (make-posn (node-x tree) (node-y tree))
                                     (make-posn (node-x (node-left tree)) (node-y (node-left tree)))))]
        [else '()]);sx
      (cond
        [(node? (node-right tree))(list (make-posn
                                     (make-posn (node-x tree) (node-y tree))
                                     (make-posn (node-x (node-right tree)) (node-y (node-right tree))))) ]
        [else '()]);dx
      (save-line (node-left tree));recall on left
      (save-line (node-right tree)))]));recall on right
  
(check-expect (save-line (set-coord SET-1 0 0 1000 100)) 
              (list (make-posn (make-posn 500 0) (make-posn 250 100))
                    (make-posn (make-posn 500 0) (make-posn 750 100))
                    (make-posn (make-posn 250 100) (make-posn 125 200))
                    (make-posn (make-posn 250 100) (make-posn 375 200))
                    (make-posn (make-posn 750 100) (make-posn 625 200))
                    (make-posn (make-posn 750 100) (make-posn 875 200)))) 

;--------------------------------------------------------------------------
;
;
;                              MAX-DEPTH
;
;
;---------------------------------------------------------------------------

;BST -> Number
;Given a BST, returns the maximal depth of the tree, meaning how many levels, the longest branch has.

(define (max-depth tree)
  (cond[(leaf? tree) 0]
       [else (+ 1 (max (max-depth (node-left tree)) (max-depth(node-right tree))))]))

(check-expect (max-depth SET-1) 3)
(check-expect (max-depth
               (list-insert-tree (list 2 4 6 9 11 13 16 19 21 24 26 28 32 34 36 40 3 8 12 17 23 27 33 39 5 15 25 35 10 30 20)))
              5)

;List<String> -> List<Number>
;Given a list of stirng, returns a list of numbers
(define (string-to-number list)
  (map string->number list)) 

(define LIST-OF-TREES  (map string-to-number (read-csv-file "multiple.csv")))
(define NUMBER-OF-TREES (length LIST-OF-TREES))
(define TREE (list-insert-tree (map string->number (first(read-csv-file "multiple.csv")))))
(define LEVEL-NUMBER (max-depth TREE))
(define LEVEL-DEPTH (/ HEIGTH  LEVEL-NUMBER))

;--------------------------------------------------------------------------
;
;
;                              SET-COORD 
;
;
;---------------------------------------------------------------------------

; BST Number Number Number Number -> BST
; Given a BST with nodes that don't have their coordinates set yet,
; and a first number, representing the y of the root node, a left-bound, a right-bound
; and a level depth
; returns the whole tree with all nodes with their coordinates set, such that the left sub-tree
; has the root in the left half and the right sub-tree have the root in the right half,
; one level below for the y coordinate

(define (set-coord tree y left-bound right-bound level-depth)
  (cond [(leaf? tree) tree]
        [else (make-node (set-coord (node-left tree) (+ y level-depth) left-bound (/ (+ left-bound right-bound)2) level-depth)
                         (node-value tree)
                         (set-coord (node-right tree) (+ y level-depth) (/ (+ left-bound right-bound)2) right-bound level-depth)
                         (/(+ left-bound right-bound)2) y
                         (node-color tree))]))

(check-expect (set-coord SET-1 0 0 1000 100)
(make-node (make-node (make-node (make-leaf) 2 (make-leaf) 125 200 0)
                      3
                      (make-node (make-leaf) 4 (make-leaf) 375 200 0) 250 100 0)
           6
           (make-node (make-node (make-leaf) 7 (make-leaf) 625 200 0)
                      8
                      (make-node (make-leaf) 9 (make-leaf) 875 200 0) 750 100 0) 500 0 0))
;--------------------------------------------------------------------------
;
;
;                              CLEAN-COLORS
;
;
;---------------------------------------------------------------------------

; BST -> BST
; Given a BST, returns the same BTS with all the colors resetted to grey
(define (clean-colors tree)
  (cond [(leaf? tree) tree]
        [else (make-node (clean-colors(node-left tree))
                         (node-value tree)
                         (clean-colors(node-right tree))
                         (node-x tree)
                         (node-y tree)
                         0)]))


(check-expect (clean-colors (make-node
                            (make-node (make-leaf) 0 (make-leaf) 0 0 1)
                            10
                            (make-node (make-leaf) 0 (make-leaf) 0 0 2)
                                       0 0 2))
                           (make-node
                            (make-node (make-leaf) 0 (make-leaf) 0 0 0)
                            10
                            (make-node (make-leaf) 0 (make-leaf) 0 0 0)
                                       0 0 0)) 

;--------------------------------------------------------------------------
;
;
;                              SET-RADIUS
;
;
;---------------------------------------------------------------------------

;Number -> Number
;Given the number of levels in the tree returns the number of nodes
;that a full tree would have in the lowest level
(define (get-number-node-lower-level x)
  (expt 2 x))

(check-expect (get-number-node-lower-level 3) 8)

;Number -> Number
;Given a number representing the maximum number of nodes that the lowest level can have,
;returns a value for the radius of the nodes, such that on that lowest level they do not
;touch each other
(define (set-radius number-of-nodes)
  (/ WIDTH  number-of-nodes))

(check-expect (set-radius 5) 300)

;BST -> Number
;given a tree returns the y margin from the top that it should have when displayed:
;It should be either the radius that a node would have when displyed, or, if that value
;is bigger than the depth (how deep each new level goes) divide by 2 ( meaning that 2 nodes would touch when displayed),
;it is simply the depth divided by 2

(define (get-y-margin tree)
  (min (set-radius (get-number-node-lower-level (max-depth tree)))
       (/ (/ HEIGTH (max-depth tree)) 2)))


; GLOBAL CONSTANTS
(define MAX-NODES-LOWER-LEVEL (get-number-node-lower-level LEVEL-NUMBER))
(define RADIUS (set-radius MAX-NODES-LOWER-LEVEL))
(define MARGIN-TOP (+ RADIUS (* 0.5 RADIUS)))
(define FONT-SIZE (-(* 2 RADIUS) (/ RADIUS 10)))
(define LIST-OF-LINES (save-line (set-coord TREE 0 0 WIDTH LEVEL-DEPTH)))         
(define LIST-OF-NODES (save-nodes (set-coord TREE 0 0 WIDTH LEVEL-DEPTH)))
(define EMPTY-CANVAS  (empty-scene WIDTH HEIGTH "transparent")) 
(define ARROW (overlay/xy (rectangle 25 25 "solid" "black") 25 -10 (rotate -90 (triangle 45 "solid" "black" ))))
(define CANVAS (overlay/xy
                (overlay/xy (text "- POST-ORDER PRESS   1" 15 "black")
                            0 15
                            (overlay/xy (text "- IN-ORDER PRESS   2" 15 "black")
                                        0 15
                            (overlay/xy (text "- PRE-ORDER PRESS    3" 15 "black")
                                        0 15
                                        (overlay/xy (text "- PRESS DOWN = 1 STEP ALGORITHM" 15 "black")
                                                    0 15
                                                    (overlay/xy (text "- PRESS RIGHT  = NEW TREE" 15 "black")
                                                                0 15
                                                                (overlay/xy (text "- RED NODE = VISITED" 15 "black")
                                                                            0 15
                                                                            (text "- GREEN NODE = MARKED" 15 "black")))))))
                                        
                                       
                -20 -20
                  EMPTY-CANVAS))
(define IN-ORDER-EVENTS (make-in-order-events TREE))
(define POST-ORDER-EVENTS (make-post-order-events TREE))
(define STARTING-APP (make-app (set-coord TREE (get-y-margin TREE) 0 WIDTH LEVEL-DEPTH) 0 0 0 LEVEL-DEPTH POST-ORDER-EVENTS RADIUS))

;--------------------------------------------------------------------------
;
;
;                              DRAW-DOTS
;
;
;---------------------------------------------------------------------------

; List<List<Number>> Application -> Image
; Given a list of list where: the first element of the list contains the x, the second y, the third the value of the node
; the fourth the color of the node, returns an image of the nodes with the corresponding position x y, value of node and color
; over the image of the lines

(define (draw-dots list app)
  (cond [(empty? list) EMPTY-CANVAS]  
        [else (place-image (overlay (text (number->string(third (first list))) (max 0 (min 255 (floor (* (app-radius app) 0.8)))) "black")
                                    (circle (draw-dots-helper (app-radius app)(app-depth app)) "solid" (get-color (fourth (first list)))))
                           (first(first list))(second(first list))
                           (draw-dots (rest list) app))])) 

;Number Number -> Number
;Given a radius and a depth selects the radius if it's not bigger than depth / 2
(define (draw-dots-helper radius depth)
  (cond [(> radius (/ depth 2)) (/ depth 2)]
        [else radius])) 

;--------------------------------------------------------------------------
;
;
;                              DRAW-LINES
;
;
;--------------------------------------------------------------------------- 

; Given a list of posn of posn, returns an image of lines having those coordinates
; List<Posn<Posn>> -> Image

(define (draw-lines list)
  (cond [(empty? list)  EMPTY-CANVAS]
        [else (add-line (draw-lines (rest list))
                        (posn-x (posn-x (first list)))
                        (posn-y (posn-x (first list)))
                        (posn-x (posn-y (first list)))
                        (posn-y (posn-y (first list)))
                        "black")]))
(define LINES (draw-lines LIST-OF-LINES))

;--------------------------------------------------------------------------
;
;
;                              GET-COLOR
;
;
;---------------------------------------------------------------------------

; Given a number, returns the corrisponding color string
; Number -> String

(define (get-color x)
  (cond [(= x 0) "grey"] 
        [(= x 1) "red"]
        [else "green"]))

(check-expect (get-color 4) "green")
(check-expect (get-color 0) "grey") 
(check-expect (get-color 1) "red")

;--------------------------------------------------------------------------
;
;
;                              SET-COLOR-TREE
;
;
;---------------------------------------------------------------------------

; Number Number BST -> BST
; Given two numbers representing the value of the node and the color of the node, and given a tree,
; returns a new tree with the node where the value is the given x, colored in the corresponding color.
; It will not have any defence against wrong values as input since it is only used with the function set-color which outputs
; are only valid numbers.

(define (set-color-tree x color tree)
  (cond [(leaf? tree) tree]
        [(= x (node-value tree)) (make-node (node-left tree) (node-value tree) (node-right tree)
                                            (node-x tree) (node-y tree) color)]
        [(< x (node-value tree)) (make-node (set-color-tree x color (node-left tree)) (node-value tree) (node-right tree)
                                            (node-x tree) (node-y tree) (node-color tree))]
        [(> x (node-value tree)) (make-node (node-left tree) (node-value tree) (set-color-tree x color (node-right tree))
                                            (node-x tree) (node-y tree) (node-color tree))])) 

;--------------------------------------------------------------------------
;
;
;                              GET-NEXT-TREE
;
;
;---------------------------------------------------------------------------

;Number List<List<Numbers>> -> BST
;Given a number, representing the n-th tree to get from the list of list of numbers
;representing the trees, build that list into a tree ready to be displayed

(define (get-next-tree x xs)
  (get-next-tree-helper (list-insert-tree(first (drop x xs))))) 

; BST -> BST
(define (get-next-tree-helper tree) 
  (set-coord tree (get-y-margin tree) 0 WIDTH (/ HEIGTH (max-depth tree))))  

;--------------------------------------------------------------------------
;
;
;                              DRAW-APP
;
;
;---------------------------------------------------------------------------

; App -> Image
; Given an app returns the image of that application

(define (draw-app app)
  (place-image (text (cond [(= (app-alg app) 0) "Post-Order" ]
                          [(= (app-alg app) 1) "In-Order"]
                          [else "Pre-Order"]) 24 "red")
               (* WIDTH 0.9) (/ HEIGTH 10)
               (overlay
                (overlay
                 (draw-dots (save-nodes(app-tree app)) app)
                 (draw-lines (save-line (app-tree app))))
                CANVAS)))

;--------------------------------------------------------------------------
;
;
;                              NEXT-EVENT
;
;
;---------------------------------------------------------------------------

; Number Application -> Application
; Given a number that represents the iterator and the app itself, returns the application with the
; iterator increased by one.

(define (next-event i app)
  (cond[( > i (- (length (app-event-list app)) 1)) app]
       [else  (next-event-helper app)])) 

; Application -> Application
(define (next-event-helper app)
  (make-app (set-color-tree (posn-x(set-color (app-iterator app) (app-event-list app)))
                                                          (posn-y(set-color (app-iterator app) (app-event-list app)))
                                                          (app-tree app)) 
                                          (+ 1(app-iterator app)) (app-alg app)
                                          (app-current-tree app) (app-depth app) (app-event-list app) (app-radius app)))

;--------------------------------------------------------------------------
;
;
;                              HANDLE-KEY
;
;
;---------------------------------------------------------------------------

; Application MouseEvent -> World
; Given an application and a mouse event, returns an application depending on the
; mouse event given.

(define (handle-key app key)
  (cond  [(string=? key "down") 
         (cond [(= (app-alg app) 0) (next-event (app-iterator app) app)]
               [(= (app-alg app) 1) (next-event (app-iterator app) app)] 
               [(= (app-alg app) 2) (next-event (app-iterator app) app)])]  
         [(string=? key "right")
          (change-tree app (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES))] 
        [(string=? key "1") (make-app
                             (clean-colors (app-tree app))
                             0 0
                             (app-current-tree app) 
                             (app-depth app)
                             (make-post-order-events (app-tree app)) (app-radius app))]
        [(string=? key "2") (make-app
                             (clean-colors (app-tree app))
                             0 1 
                             (app-current-tree app) 
                             (app-depth app)
                             (make-in-order-events (app-tree app))(app-radius app))]
        [(string=? key "3") (make-app
                             (clean-colors (app-tree app))
                             0 2 
                             (app-current-tree app)
                             (app-depth app) 
                             (make-pre-order-events (app-tree app)) (app-radius app))]
        [else app]))

;--------------------------------------------------------------------------
;
;
;                              CHANGE-TREE
;
;
;---------------------------------------------------------------------------

; Application BST -> Application
; Given an Application and a tree, returns a new application with the parameters which
; depend on the algorithm to execute

(define (change-tree app new-tree)
  (cond [(= (app-alg app) 0)  
         (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES)
                                 (/ HEIGTH (max-depth new-tree))
                                 (make-post-order-events new-tree)
                                 (set-radius (get-number-node-lower-level (max-depth new-tree)) ) )]
        [(= (app-alg app) 1)
          (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES)
                                 (/ HEIGTH (max-depth new-tree))
                                 (make-in-order-events new-tree)
                                 (set-radius (get-number-node-lower-level (max-depth new-tree)) ))]
        [(= (app-alg app) 2) (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES)
                                 (/ HEIGTH (max-depth new-tree))
                                 (make-pre-order-events new-tree)
                                 (set-radius (get-number-node-lower-level (max-depth new-tree)) ))]
        ))

;--------------------------------------------------------------------------
;
;
;                              BIG-BANG
;
;
;---------------------------------------------------------------------------

; Nothing -> Image
; Main Function, given any input, starts the big-bang function
; creating a graphical interface, allowing the user to execute the
; program.

(define (main _)
  (big-bang STARTING-APP 
    [to-draw draw-app]
    ;[on-mouse handle-mouse]
    [on-key handle-key]
    )
  )  
