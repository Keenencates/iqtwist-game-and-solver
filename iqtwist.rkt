#lang racket
(require 2htdp/image)

;graphical constants
(define ROWS 4)
(define COLS 8)
(define CELL-WIDTH 30)
(define CELL-HEIGHT 30)
(define CELL-CENTER-X (/ CELL-WIDTH 2))
(define CELL-CENTER-Y (/ CELL-HEIGHT 2))
(define INNER-OFFSET 7)
(define INNER-RADIUS (- (/ CELL-WIDTH 2) INNER-OFFSET))
(define BOARD-WIDTH (* CELL-WIDTH COLS)) 
(define BOARD-HEIGHT (* CELL-HEIGHT ROWS))

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

;graphical board
(define EMPTY-SCENE (empty-scene BOARD-WIDTH BOARD-HEIGHT))
(define EMPTY-CELL-BOARD-ROW (apply beside (build-list COLS (const EMPTY-CELL))))
(define EMPTY-CELL-BOARD (apply above (build-list ROWS (const EMPTY-CELL-BOARD-ROW))))

(define CELL-TABLE (hash "e" EMPTY-CELL
                         "rp" (make-peg-cell "red")
                         "gp" (make-peg-cell "green")
                         "bp" (make-peg-cell "blue")
                         "yp" (make-peg-cell "yellow")))

(define (get-cell c)
  (hash-ref CELL-TABLE c))

(define (make-cell-board-row board-row)
  (apply beside (map get-cell board-row)))

(define (make-cell-board board)
  (apply above (map make-cell-board-row board)))

;non-graphical board
(define EMPTY-BOARD (build-list ROWS (const (build-list COLS (const "e")))))

;pieces and moves
;TODO -> UPDATE TO NEW GRAPHICAL SYSTEM
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
