#lang typed/racket

;(require rackunit)
(require profile)
;(require "gui-iqtwist.rkt")

;TODO -- add unit testing
;     -- move generation
;     -- graphical piece bank
;     -- grid coordinates
;     -- mouse interaction

;Board constants

(struct Point ([x : Number] [y : Number] [pc : String]))
(define-type Board (-> Listof(Listof(Int))))
(define-type Piece (-> Listof(Listof(Point))))

(: ROWS Number)
(define ROWS 4)

(: COLS Number)
(define COLS 8)

;PIECE CONSTANTS
(: RED1 Piece)
(define RED1 (list 'RED1 (list 0 0 "rs") (list 0 1 "re") (list 1 1 "rs") (list 2 1 "re")))

(: RED2 Piece)
(define RED2 (list 'RED2 (list 0 0 "rs") (list 1 0 "re") (list 1 1 "rs") (list 2 1 "rs")))

(: BLUE1 Piece)
(define BLUE1 (list 'BLUE1 (list 0 0 "bs") (list 1 0 "bs") (list 2 0 "bs")(list 1 1 "bs") (list 2 1 "be")))

(: BLUE2 Piece
(define BLUE2 (list 'BLUE2 (list 0 0 "bs") (list 1 0 "be") (list 2 0 "bs") (list 3 0 "bs")))

(: GREEN1 Piece)
(define GREEN1 (list 'GREEN1(list 0 0 "gs") (list 0 1 "gs") (list 0 2 "gs") (list 1 1 "ge")))

(: GREEN2 Piece)
(define GREEN2 (list 'GREEN2(list 0 0 "ge") (list 0 1 "ge") (list 1 1 "gs")))

(: YELLOW1 Piece)
(define YELLOW1 (list 'YELLOW1 (list 0 0 "ye") (list 1 0 "ys") (list 2 0 "ys")))

(: YELLOW2 Piece)
(define YELLOW2 (list 'YELLOW2 (list 0 0 "ye") (list 1 0 "ye") (list 1 1 "ys") (list 1 2 "ys") (list 2 2 "ye")))

(: PIECE-LIST Listof(Piece))
(define PIECE-LIST (list RED1 RED2 BLUE1 BLUE2 GREEN1 GREEN2 YELLOW1 YELLOW2))

;BOARD
(define COLOR-SYMBOLS (list "r" "g" "b" "y"))
(define PEG-SYMBOL "p")
(define COVER-SYMBOL "s")
(define OPEN-COVER-SYMBOL "e")
(define OPEN-COVER-PEG-SYMBOL "o")

(define PEG-SYMBOLS (map (lambda ([x : String]) (string-append x PEG-SYMBOL)) COLOR-SYMBOLS))
(define COVER-SYMBOLS (map (lambda ([x : String]) (string-append x COVER-SYMBOL)) COLOR-SYMBOLS))
(define OPEN-COVER-SYMBOLS (map (lambda ([x : String]) (string-append x OPEN-COVER-SYMBOL)) COLOR-SYMBOLS))
(define OPEN-COVER-PEG-SYMBOLS (map (lambda ([x : String]) (string-append x OPEN-COVER-PEG-SYMBOL)) COLOR-SYMBOLS))

(define EMPTY-BOARD (build-list ROWS (const (build-list COLS (const "e")))))

(:get-board (-> Int Int Board String))
(define (get-board x y board) (list-ref (list-ref board x) y))

;MOVES
;;These do not need error checking, only move generator
(define (which-piece board coord piece)
  (if (is-open-cover-peg-place? (+ (first coord)
                                   (first piece))
                                (+ (second coord)
                                   (second piece))
                                board
                                (third piece))
      (string (string-ref (third piece) 0) #\o)
      (third piece)))

(define (place-single-piece board coord piece)
  (let ([new-piece (which-piece board coord piece)])
    (list-set board
              (+ (first coord)(first piece))
              (list-set (list-ref board
                                  (+ (first coord) (first piece)))
                        (+ (second coord) (second piece))
                        new-piece))))

(define (place-multi-piece board coord pieces)
  (foldl (lambda (p b)
           (place-single-piece b coord p))
           board
           pieces))

;move error checking

;;A move is a pair, the location to place origin point,
;;and a list of relative piece segments coords to piece
;;origin.

;;Need to check for errors when a move would be invalid.
;;Piece construction should already be validated.
(define LEFT-BOUNDARY 0)
(define RIGHT-BOUNDARY 8)
(define TOP-BOUNDARY 4)
(define BOTTOM-BOUNDARY 0)

(define (in-bounds? x y)
  (and (< y RIGHT-BOUNDARY)
       (>= y LEFT-BOUNDARY)
       (< x TOP-BOUNDARY)
       (>= x BOTTOM-BOUNDARY)))

(define (in-bounds-placement? x0 y0 x1 y1)
  (in-bounds? (+ x0 x1) (+ y0 y1)))

(define (is-empty? x y board)
  (equal? "e" (get-board x y board)))

(define (is-peg? x y board)
  (member (get-board x y board) PEG-SYMBOLS))

(define (is-same-color? peg-type piece-type)
  (equal? (string-ref peg-type 0)
          (string-ref piece-type 0)))

(define (is-open-cover-peg? piece)
  (member piece OPEN-COVER-PEG-SYMBOLS))

(define (is-open-cover-peg-place? x y board type)
  (and (is-peg? x y board)
       (member type OPEN-COVER-SYMBOLS)
       (is-same-color? (get-board x y board)
                       type)))

(define (can-place-single? x0 y0 board x1 y1 type)
  (let ([x (+ x0 x1)]
       [y (+ y0 y1)])
    (and (in-bounds? x y)
         (or (is-empty? x y board)
             (is-open-cover-peg-place? x y board type)))))

(define (can-place? location board piece)
  (andmap (lambda (p)
            (can-place-single? (first location)
                               (second location)
                               board
                               (first p)
                               (second p)
                               (third p)))
          piece))


;move generator

(define EMPTY-STATE (list EMPTY-BOARD PIECE-LIST))

(define (make-coords x) (build-list COLS (lambda (i) (list x i))))

(define BOARD-COORDS (foldr append '() (build-list ROWS (lambda (i)(make-coords i)))))

(define (generate-location-moves board rotation sym)
  (foldl (lambda (p result)
           (cond
             [(can-place? p board rotation) (cons (list sym p rotation) result)]
             [else result]))
         '()
         BOARD-COORDS))

(define (generate-transformation-moves board piece)
  (foldl (lambda (rotation result)
           (append result (generate-location-moves board rotation (first piece))))
         '()
         (generate-all-transformations (rest piece))))

(define (generate-all-moves state)
  (foldl (lambda (piece result)
           (append result (generate-transformation-moves (first state) piece)))
         '()
         (second state)))

(define (is-move-covering-peg? board move)
  (let ([peg-covers (filter is-open-cover-peg? (flatten board))]
        [next-board (place-multi-piece board (first move) (second move))])
    (let ([new-covers (filter is-open-cover-peg? (flatten next-board))])
      (> (length new-covers) (length peg-covers)))))

;pieces
(define (single-piece-flip-horizontal piece) (list (- (first piece)) (second piece) (third piece)))
(define (multi-piece-flip-horizontal piece) (map single-piece-flip-horizontal piece))
(define (single-piece-rotate-right piece) (list (second piece) (- (first piece)) (third piece)))
(define (multi-piece-rotate-right piece) (map single-piece-rotate-right piece))

(define (flip piece) (multi-piece-flip-horizontal piece))
(define (rotate piece) (multi-piece-rotate-right piece))

(define (rotate-90 piece) (rotate piece))
(define (rotate-180 piece) (rotate (rotate-90 piece)))
(define (rotate-270 piece) (rotate (rotate-180 piece)))

(define (generate-rotations piece)
  (list piece (rotate-90 piece)(rotate-180 piece)(rotate-270 piece)))

(define (generate-all-transformations piece)
  (set->list(list->set(let ([ipiece (flip piece)])
    (append (generate-rotations piece) (generate-rotations ipiece))))))


;AI
;; nodes/states are (board 
(define (prune-non-peg-covering-edges node edges)
  (let ([pruned-edges (filter (lambda (mv)
                                (is-move-covering-peg? (first node) (rest mv)))
                              edges)])
    (if (empty? pruned-edges)
        edges
        pruned-edges)))

(define (explore-node state) (set->list (list->set (generate-all-moves state))))

(define (traverse-edge state move)
  (list (place-multi-piece (first state) (first (rest move)) (second (rest move))) (remove (first move) (second state))))

(define (expand-node state)
  (map (lambda (mv) (traverse-edge state mv)) (explore-node state)))

(define (pruned-explore-node state)
  (prune-non-peg-covering-edges state (explore-node state)))

(define (pruned-expand-node state)
  (map (lambda (mv) (delay (traverse-edge state mv))) (pruned-explore-node state)))

(define (DFS root-state)
  (DFS-helper (list root-state)))

(define (DFS-helper frontier)
  (let ([node (if (not (empty? frontier))
                  (force (first frontier))
                  '())])
    (cond
      [(empty? node) '()]
      [(empty? (second node)) (first )]
      [else (DFS-helper (append (pruned-expand-node node) (rest frontier)))])))

(define tb1 (list (list "e" "e" "e" "e" "e" "e" "e" "e")
                  (list "e" "e" "yp" "e" "e" "bp" "e" "e")
                  (list "e" "e" "e" "e" "e" "e" "e" "e")
                  (list "e" "e" "gp" "e" "e" "rp" "e" "e")))
    
;(define ts1 (list tb1 PIECE-LIST))
;(define soln (DFS ts1))
;(display soln)
