#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;TODO -- add unit testing
;     -- move generation
;     -- graphical piece bank
;     -- grid coordinates
;     -- mouse interaction

;graphical constants
(define ROWS 4)
(define COLS 8)
(define CELL-WIDTH 30)
(define CELL-HEIGHT 30)
(define CELL-CENTER-X (/ CELL-WIDTH 2))
(define CELL-CENTER-Y (/ CELL-HEIGHT 2))
(define INNER-OFFSET 7)
(define INNER-RADIUS (- (/ CELL-WIDTH 2) INNER-OFFSET))
(define OUTER-OFFSET 2)
(define OUTER-RADIUS (- (/ CELL-WIDTH 2) OUTER-OFFSET))
(define BOARD-WIDTH (* CELL-WIDTH COLS)) 
(define BOARD-HEIGHT (* CELL-HEIGHT ROWS))
(define (THICK-PEN color) (make-pen color 4 "solid" "round" "round"))

;graphical cell-types
;;; TODO change circle to ellipsoid
(define EMPTY-CELL (overlay (circle INNER-RADIUS
                                    "outline"
                                    "white")
                            (rectangle CELL-WIDTH
                                       CELL-HEIGHT
                                       "solid"
                                       "black")))

(define (make-peg-cell color) (overlay (circle INNER-RADIUS
                                               "solid"
                                               color)
                                  (rectangle CELL-WIDTH
                                             CELL-HEIGHT
                                             "solid"
                                             "black")))

(define (make-open-cover-peg-cell color)
  (overlay (circle OUTER-RADIUS "outline" (THICK-PEN color))
           (make-peg-cell color)))

(define (make-solid-cover-cell color)
  (overlay (circle (+ OUTER-RADIUS 1) "solid" color) EMPTY-CELL))

(define (make-open-cover-cell color)
  (overlay (circle OUTER-RADIUS "outline" (THICK-PEN color))
           EMPTY-CELL))

;graphical board
(define EMPTY-SCENE (empty-scene BOARD-WIDTH BOARD-HEIGHT))
(define EMPTY-CELL-BOARD-ROW (apply beside (build-list COLS (const EMPTY-CELL))))
(define EMPTY-CELL-BOARD (apply above (build-list ROWS (const EMPTY-CELL-BOARD-ROW))))

(define CELL-TABLE (hash "e" EMPTY-CELL
                         "rp" (make-peg-cell "red")
                         "gp" (make-peg-cell "green")
                         "bp" (make-peg-cell "blue")
                         "yp" (make-peg-cell "yellow")
                         "ro" (make-open-cover-peg-cell "red")
                         "go" (make-open-cover-peg-cell "green")
                         "bo" (make-open-cover-peg-cell "blue")
                         "yo" (make-open-cover-peg-cell "yellow")
                         "rs" (make-solid-cover-cell "red")
                         "gs" (make-solid-cover-cell "green")
                         "bs" (make-solid-cover-cell "blue")
                         "ys" (make-solid-cover-cell "yellow")
                         "re" (make-open-cover-cell "red")
                         "ge" (make-open-cover-cell "green")
                         "be" (make-open-cover-cell "blue")
                         "ye" (make-open-cover-cell "yellow")))

(define (get-cell c)
  (hash-ref CELL-TABLE c))

(define (make-cell-board-row board-row)
  (apply beside (map get-cell board-row)))

(define (make-cell-board board)
  (apply above (map make-cell-board-row board)))

;non-graphical board
(define EMPTY-BOARD (build-list ROWS (const (build-list COLS (const "e")))))

;moves
(define (place-single-piece board coord piece)
  (list-set board
            (+ (first coord)(first piece))
            (list-set (list-ref board
                                (+ (first coord) (first piece)))
                      (+ (second coord) (second piece))
                      (third piece))))

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
  (and (x < RIGHT-BOUNDARY)
       (x >= LEFT-BOUNDARY)
       (x < TOP-BOUNDARY)
       (x >= BOTTOM-BOUNDARY)))

(define (in-bounds-placement? x0 y0 x1 y1)
  (in-bounds? (+ x0 x1) (+ y0 y1)))

;;TODO
(define (make-move location piece) 1)


;move generator
;;TODO
(define (generate-moves-single-piece board piece) 1)

;graphical pieces
;; TODO - add a piece bank for graphical display

;pieces
;; TODO - add piece construction and validation.
(define (single-piece-flip-horizontal piece) (list (- (first piece)) (second piece) (third piece)))
(define (multi-piece-flip-horizontal piece) (map single-piece-flip-horizontal piece))
(define (single-piece-rotate-right piece) (list (second piece) (- (first piece)) (third piece)))
(define (multi-piece-rotate-right piece) (map single-piece-rotate-right piece))

(define red1 (list (list 0 0 "rs") (list 0 1 "re") (list 1 1 "rs") (list 2 1 "re")))

;gui
;;TODO ADD BOUNDS CHECKING
(define (next-state state move)
  (place-multi-piece state (first move)(second move)))

(define initial-state EMPTY-BOARD)
(define (draw-handler state) (make-cell-board state))

(define move1 (list '(0 0) red1))
;;TODO - Dummy key handler
(define (key-handler state a-key)
  (cond
    [(key=? a-key "up")(next-state state move1)]))

(big-bang initial-state
          (to-draw draw-handler)
          (on-key key-handler))
