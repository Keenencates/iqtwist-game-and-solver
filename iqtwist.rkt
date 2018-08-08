#lang racket

(define (print-seperator cols)
  (pretty-print (string-join (build-list cols (lambda (x) "-")) "")))

(define (make-board-row cols)
  (build-list cols (lambda (x) "o")))

(define (make-board rows cols)
  (build-list rows (lambda (x)
                     (make-board-row cols))))

(define (print-board-row row)
  (pretty-print
   (string-join row " ")))

(define (print-board board)
  (for-each print-board-row board))

(define (make-square-piece mark)
  (list (list 0 0 mark)
        (list 0 1 mark)
        (list 1 0 mark)
        (list 1 1 mark)))

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

(print-board (make-board 4 8))
(print-seperator 15)
(print-board (place-single-piece (make-board 4 8) '(0 0) '(0 0 "x")))
(print-seperator 15)
(print-board (place-single-piece (make-board 4 8) '(0 0) '(1 1 "x")))
(print-seperator 15)
(print-board (place-single-piece (make-board 4 8) '(1 1) '(1 1 "x")))
(print-seperator 15)
(make-square-piece "x")
(print-seperator 15)
(print-board (place-multi-piece (make-board 4 8) '(0 0) (make-square-piece "x")))