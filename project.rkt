;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;---------------------------------------------------------------------------
;
;
;                         TREE ALGORITHM VISUALIZER
;
;
;---------------------------------------------------------------------------
;GLOBAL CONSTANTS
(define HEIGTH 500)
(define WIDTH 1500)




; Libraries Requiring

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)




; a Node is one of :
; (make-leaf)
; (make-node BST Value BST x y Color)
; a value is a number
; x y are coordinates of that node on a canvas
; different a color is a number and it depends of the state of the node.
; NodeColor is one of:
; - 0 for white
; - 1 for red
; - 2 for green
; a App is a (make-app List<List<Number>> Number Number ) where the list of list is a rappresentation
; of nodes' parameters (x y value color), and the iterator is a NatNumber
; representing at which step the application is.
; Alg is a Natural Number between [0,2] that represent a function.
; Current-Tree is a Natural Number between 


(define-struct node [left value right x y color])
(define-struct leaf [])
(define-struct app [tree iterator alg current-tree depth event-list])



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
; Given a list of numbers, return a BST (Binary Search Tree) 

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

;(save-nodes (set-coord SET-1  0 0 1000))
(check-expect (save-nodes SET-1) (list (list 0 0 6 0) (list 0 0 3 0) (list 0 0 2 0)
                                       (list 0 0 4 0) (list 0 0 8 0) (list 0 0 7 0) (list 0 0 9 0))) 



;---------------------------------------------------------------------------
;
;
;                             FOO FUNCTION
;
;
;---------------------------------------------------------------------------


;BST -> List<Number>
;Given a Tree returns a list such that:
; let i be an index of the list
; if list(i) is not present in list(0)-list(i-1) it means that the node with the number=list(i) has to be coloured in red
; else the node with the number=list(i) has to be coloured in green.
;The result is that a node is coloured in red when visited, and in green when it is the result of the in-order traversal.

(define (foo tree)
  (cond [(leaf? tree) ' ()]
        [else (append (list(node-value tree))
                      (foo (node-left tree))
                      (list(node-value tree))
                      (foo (node-right tree))
                      )]))

;(foo SET-1)
(check-expect (foo SET-1) 
              (list 6 3 2 2 3 4 4 6 8 7 7 8 9 9))
;---------------------------------------------------------------------------
;
;
;                             FOO-POST-ORDER
;
;
;---------------------------------------------------------------------------

;BST -> List<Number>

(define (foo-post-order tree)
  (cond [(leaf? tree) ' ()]
        [else (append (list(node-value tree))
                      (foo-post-order (node-left tree))
                      (foo-post-order (node-right tree))
                      (list(node-value tree)))]))

(check-expect (foo-post-order SET-1) 
              (list 6 3 2 2 4 4 3 8 7 7 9 9 8 6))

;---------------------------------------------------------------------------
;
;
;                             FOO-PRE-ORDER
;
;
;---------------------------------------------------------------------------

; BST -> List<Number>

(define (foo-pre-order tree)
  (cond [(leaf? tree) '()]
        [else (append (list(node-value tree))
                      (list(node-value tree))
                      (foo-pre-order (node-left tree))
                      (foo-pre-order (node-right tree)))]))

(check-expect (foo-pre-order SET-1) 
              (list 6 6 3 3 2 2 4 4 8 8 7 7 9 9 ))
                      
                           



;---------------------------------------------------------------------------
;
;
;                              CONTAINS? 
;
;
;---------------------------------------------------------------------------


; Number List<Number> -> Boolean
; Given a number and a list of numbers, returns a boolean, checks whether a number is contained in a list

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
; Given a number and a list, return the last n elements of the list

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
; Helper function for the one above which required to have the list dropping some indices multiple times
; is not passed as a parameter

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

; BST Number Number Number -> BST
; Given a BST with nodes that don't have their coordinates set yet,
; and a first pair or numbers, representing the x and y of the root node
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
(define (set-radius number-of-nodes margin)
  (- (/ WIDTH  number-of-nodes) margin) )

(check-expect (set-radius 5 10) 290)


; GLOBAL CONSTANTS
(define MAX-NODES-LOWER-LEVEL (get-number-node-lower-level LEVEL-NUMBER))
(define RADIUS (set-radius MAX-NODES-LOWER-LEVEL 10))
(define MARGIN-TOP (+ RADIUS (* 0.5 RADIUS)))
(define FONT-SIZE (-(* 2 RADIUS) (/ RADIUS 10)))
(define LIST-OF-LINES (save-line (set-coord TREE MARGIN-TOP 0 WIDTH LEVEL-DEPTH)))         
(define LIST-OF-NODES (save-nodes (set-coord TREE MARGIN-TOP 0 WIDTH LEVEL-DEPTH)))
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
                                                                            (overlay/xy (text "- GREEN NODE = MARKED" 15 "black")
                                        
                                       ( - WIDTH 200) 0
                                       ARROW)))))))
                  -20 -20
                                             EMPTY-CANVAS))
(define FOO (foo TREE))
(define FOO-POST-ORDER (foo-post-order TREE))
(define STARTING-APP (make-app (set-coord TREE MARGIN-TOP 0 WIDTH LEVEL-DEPTH) 0 0 0 LEVEL-DEPTH FOO-POST-ORDER))




;--------------------------------------------------------------------------
;
;
;                              DRAW-DOTS
;
;
;---------------------------------------------------------------------------

; List<List<Number>> -> Image
; Given a list of list where: the first element of the list contains the x, the second y, the third the value of the node
; the fourth the color of the node, returns an image of the nodes with the corresponding position x y, value of node and color
; over the image of the lines

(define (draw-dots list)
  (cond [(empty? list) EMPTY-CANVAS]
        [else (place-image (overlay (text (number->string(third (first list))) (if (> FONT-SIZE 255) 255 (floor FONT-SIZE)) "black")
                                    (circle RADIUS "solid" (get-color (fourth (first list)))))
                           (first(first list))(second(first list))
                           (draw-dots (rest list)))]))



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

; Given a number, returns a string to define the color of the nodes
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
; Given two number defining the value of the node and the color of the node, and given a tree,
; returns a new tree with the node where the value is the given x colored in the color of which color stands.
; It will not have any defense against wrong value as input since it is linked to the function set-color which outputs
; are only valid numbers.

(define (set-color-tree x color tree)
  (cond [(leaf? tree) tree]
        [(= x (node-value tree)) (make-node (node-left tree) (node-value tree) (node-right tree)
                                            (node-x tree) (node-y tree) color)]
        [(< x (node-value tree)) (make-node (set-color-tree x color (node-left tree)) (node-value tree) (node-right tree)
                                            (node-x tree) (node-y tree) (node-color tree))]
        [(> x (node-value tree)) (make-node (node-left tree) (node-value tree) (set-color-tree x color (node-right tree))
                                            (node-x tree) (node-y tree) (node-color tree))]))


;(draw-dots (save-nodes (set-color-tree 8 2 (set-coord SET-1 MARGIN-TOP 0 1000))))

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
  (get-next-tree-helper x xs (list-insert-tree(first (drop x xs)))))


(define (get-next-tree-helper x xs tree)
  (set-coord tree MARGIN-TOP 0 WIDTH (/ HEIGTH (max-depth tree))))




;--------------------------------------------------------------------------
;
;
;                  k            DRAW-APP
;
;
;---------------------------------------------------------------------------

; Given an app, returns an image
; App -> Image

(define (draw-app app)
  (place-image (text (cond [(= (app-alg app) 0) "Post-Order" ]
                          [(= (app-alg app) 1) "In-Order"]
                          [else "Pre-Order"]) 24 "red")
               (* WIDTH 0.9) (/ HEIGTH 10)
               (overlay
                (overlay
                 (draw-dots (save-nodes(app-tree app)))
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
; Given a number that identifies the iterator and the app itself, returns the application with the
; iterator increased by one.

(define (next-event i app)
  (cond[( > i (- (length (app-event-list app)) 1)) app]
       [else  (next-event-helper app)])) 

(define (next-event-helper app)
  (make-app (set-color-tree (posn-x(set-color (app-iterator app) (app-event-list app)))
                                                          (posn-y(set-color (app-iterator app) (app-event-list app)))
                                                          (app-tree app)) 
                                          (+ 1(app-iterator app)) (app-alg app)
                                          (app-current-tree app) (app-depth app) (app-event-list app)))

;--------------------------------------------------------------------------
;
;
;                              HANDLE-KEY
;
;
;---------------------------------------------------------------------------


;  World MouseEvent -> World
;  Pressing button-down, it triggers the function make-app and returns a make-app

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
                             (foo-post-order (app-tree app)))]
        [(string=? key "2") (make-app
                             (clean-colors (app-tree app))
                             0 1 
                             (app-current-tree app)
                             (app-depth app)
                             (foo (app-tree app)))]
        [(string=? key "3") (make-app
                             (clean-colors (app-tree app))
                             0 2 
                             (app-current-tree app)
                             (app-depth app)
                             (foo-pre-order (app-tree app)))]
        [else app]))

;--------------------------------------------------------------------------
;
;
;                              CHANGE-TREE
;
;
;---------------------------------------------------------------------------

; Application BST -> BST
; Given an Application and a tree, returns a new tree depends of the algorithm to execute

(define (change-tree app new-tree)
  (cond [(= (app-alg app) 0) 
         (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES) (app-depth app)
                                 (foo-post-order new-tree))]
        [(= (app-alg app) 1)
          (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES) (app-depth app)
                                 (foo new-tree))]
        [(= (app-alg app) 2) (make-app
                                 (get-next-tree (modulo (+ (app-current-tree app) 1) NUMBER-OF-TREES) LIST-OF-TREES)
                                 0
                                 (app-alg app)
                                 (modulo (+ (app-current-tree app) 1)  NUMBER-OF-TREES) (app-depth app)
                                 (foo-pre-order new-tree))]
        ))

;--------------------------------------------------------------------------
;
;
;                              BIG-BANG
;
;
;---------------------------------------------------------------------------

; Nothing -> Image

(define (main _)
  (big-bang STARTING-APP 
    [to-draw draw-app]
    ;[on-mouse handle-mouse]
    [on-key handle-key]
    )
  )  


