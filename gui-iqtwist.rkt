#lang racket

(provide draw-handler)
(require 2htdp/image)
(require 2htdp/universe)
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

(define (draw-handler state) (make-cell-board state))