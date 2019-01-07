#lang typed/racket

;(require rackunit)
(require profile)
(provide 

;TODO -- add unit testing
;     -- move generation
;     -- graphical piece bank
;     -- grid coordinates
;     -- mouse interaction

(struct Point ([row : Integer] [col : Integer]) #:transparent)
(struct Segment ([point : Point] [color : Symbol] [geom : Symbol]) #:transparent)
(define-type Board (Listof Segment))
(struct Piece ([name : Symbol] [segments : (Listof Segment)]) #:transparent)

(: equal-piece? (-> Piece Piece Boolean))
(define (equal-piece? p1 p2)
  (let ([pts1 : (Listof Point) (map (lambda ([segment : Segment])(Segment-point segment)) (Piece-segments p1))]
        [pts2 : (Listof Point) (map (lambda ([segment : Segment])(Segment-point segment)) (Piece-segments p2))])
    (and (equal? (Piece-name p1) (Piece-name p2))
         (andmap (lambda ([pt : Point]) (cond ((member pt pts2) #t) (else #f))) pts1))))

(struct State ([board : Board] [piece-list : (Listof Piece)][place-pieces : (Listof Symbol)]) #:transparent)

(: make-point (-> (Listof Integer) Point))
(define (make-point xy)
  (Point (first xy) (second xy)))

(: point-eqv? (-> Point Point Boolean))
(define (point-eqv? pt1 pt2)
  (and (eqv? (Point-row pt1) (Point-row pt2))
       (eqv? (Point-col pt1) (Point-col pt2))))

;PIECE CONSTANTS
(: RED1 Piece)
(define RED1 (Piece 'RED1 (list (Segment (Point 0 0) 'r 's)
                                (Segment (Point 0 1) 'r 'o)
                                (Segment (Point 1 1) 'r 's)
                                (Segment (Point 2 1) 'r 'o))))

(: RED2 Piece)
(define RED2 (Piece 'RED2 (list (Segment (Point 0 0) 'r 's)
                                (Segment (Point 1 0) 'r 'o)
                                (Segment (Point 1 1) 'r 's)
                                (Segment (Point 2 1) 'r 's))))

(: BLUE1 Piece)
(define BLUE1 (Piece 'BLUE1 (list (Segment (Point 0 0) 'b 's)
                                  (Segment (Point 1 0) 'b 's)
                                  (Segment (Point 2 0) 'b 's)
                                  (Segment (Point 1 1) 'b 's)
                                  (Segment (Point 2 1) 'b 'o))))

(: BLUE2 Piece)
(define BLUE2 (Piece 'BLUE2 (list (Segment (Point 0 0) 'b 's)
                                  (Segment (Point 1 0) 'b 'o)
                                  (Segment (Point 2 0) 'b 's)
                                  (Segment (Point 3 0) 'b 's))))

(: GREEN1 Piece)
(define GREEN1 (Piece 'GREEN1 (list (Segment (Point 0 0) 'g 's)
                                    (Segment (Point 0 1) 'g 's)
                                    (Segment (Point 0 2) 'g 's)
                                    (Segment (Point 1 1) 'g 'o))))

(: GREEN2 Piece)
(define GREEN2 (Piece 'GREEN2 (list (Segment (Point 0 0) 'g 'o)
                                    (Segment (Point 0 1) 'g 'o)
                                    (Segment (Point 1 1) 'g 's))))

(: YELLOW1 Piece)
(define YELLOW1 (Piece 'YELLOW1 (list (Segment (Point 0 0) 'y 'o)
                                      (Segment (Point 1 0) 'y 's)
                                      (Segment (Point 2 0) 'y 's))))

(: YELLOW2 Piece)
(define YELLOW2 (Piece 'YELLOW2 (list (Segment (Point 0 0) 'y 'o)
                                      (Segment (Point 1 0) 'y 'o)
                                      (Segment (Point 1 1) 'y 's)
                                      (Segment (Point 1 2) 'y 's)
                                      (Segment (Point 2 2) 'y 'o))))

(: PIECE-LIST (Listof Piece))
(define PIECE-LIST (list RED1 RED2 BLUE1 BLUE2
                         GREEN1 GREEN2 YELLOW1 YELLOW2))

;PIECE HELPER FUNCTIONS

(: get-piece-segment (-> Piece Integer Segment))
(define (get-piece-segment piece n)
  (list-ref (Piece-segments piece) n))

(: flip-point (-> Point Point))
(define (flip-point pt)
  (Point (- (Point-row pt)) (Point-col pt)))

(: rotate-point (-> Point Point))
(define (rotate-point pt)
  (Point (Point-col pt) (- (Point-row pt))))

(: transform-piece (-> Piece (-> Point Point) Piece))
(define (transform-piece piece transform-point)
  (Piece (Piece-name piece) (map (lambda ([segment : Segment])
                                   (Segment (transform-point (Segment-point segment))
                                            (Segment-color segment)
                                            (Segment-geom segment)))
                                 (Piece-segments piece))))

(: rotate-90 (-> Piece Piece))
(define (rotate-90 piece) (transform-piece piece rotate-point))

(: flip (-> Piece Piece))
(define (flip piece) (transform-piece piece flip-point)) 

(: generate-transformations (-> (Listof Piece) (Listof Piece)))
(define (generate-transformations pieces)
  (foldl (lambda ([piece : Piece][res : (Listof Piece)])
           (let ([fpiece : Piece (flip piece)])
             (append res (remove-duplicates (list piece
                                                  (rotate-90 piece)
                                                  (rotate-90 (rotate-90 piece))
                                                  (rotate-90 (rotate-90 (rotate-90 piece)))
                                                  fpiece
                                                  (rotate-90 fpiece)
                                                  (rotate-90 (rotate-90 fpiece))
                                                  (rotate-90 (rotate-90 (rotate-90 fpiece))))))))
         '()
         pieces))

;BOARD CONSTANTS
(: ROWS Integer)
(define ROWS 4)

(: COLS Integer)
(define COLS 8)

(: LEFT-BOUNDARY Integer)
(define LEFT-BOUNDARY 0)

(: RIGHT-BOUNDARY Integer)
(define RIGHT-BOUNDARY COLS)

(: TOP-BOUNDARY Integer)
(define TOP-BOUNDARY ROWS)

(: BOTTOM-BOUNDARY Integer)
(define BOTTOM-BOUNDARY 0)

(: in-bounds? (-> Point Boolean))
(define (in-bounds? point)
  (let ([row : Integer (Point-row point)]
        [col : Integer (Point-col point)])
    (and (< col RIGHT-BOUNDARY)
         (>= col LEFT-BOUNDARY)
         (< row TOP-BOUNDARY)
         (>= row BOTTOM-BOUNDARY))))

(: BOARD-COORDS (Listof Point))
(define BOARD-COORDS (map make-point (cartesian-product (range ROWS) (range COLS))))

(: EMPTY-BOARD Board)
(define EMPTY-BOARD (map (lambda ([pt : Point]) (Segment pt 'e 'e)) BOARD-COORDS))

(: EMPTY-STATE State)
(define EMPTY-STATE (State EMPTY-BOARD PIECE-LIST '()))

;BOARD HELPER FUNCTIONS
(: 2d->1d (-> Point Integer))
(define (2d->1d point)
  (+ (* COLS (Point-row point)) (Point-col point)))

(: get-board (-> Point Board Segment))
(define (get-board point board) (list-ref board (2d->1d point)))

;MOVES

(: relative-coord (-> Point Point Point))
(define (relative-coord pt1 pt2)
  (Point (+ (Point-row pt1)(Point-row pt2))
         (+ (Point-col pt1)(Point-col pt2))))

(: segment-coord (-> Point Segment Point))
(define (segment-coord board-origin segment)
  (relative-coord board-origin (Segment-point segment)))

(: update-segment-coords (-> Point Segment Board Segment))
(define (update-segment-coords board-origin segment board)
  (Segment (segment-coord board-origin segment)
           (Segment-color segment)
           (if (in-bounds? (segment-coord board-origin segment))
               (if (not (is-peg? (get-board (segment-coord board-origin segment) board)))
                   's
                   (Segment-geom segment))
               's)))

(: update-piece-coords (-> Point Piece Board Piece))
(define (update-piece-coords coord piece board)
  (Piece (Piece-name piece) (map (lambda ([segment : Segment])
                                   (update-segment-coords coord segment board))
                                 (Piece-segments piece))))

(: is-peg? (-> Segment Boolean))
(define (is-peg? segment)
  (eqv? (Segment-geom segment) 'p))

(: is-open? (-> Segment Boolean))
(define (is-open? segment)
  (eqv? (Segment-geom segment) 'o))

(: is-empty? (-> Segment Boolean))
(define (is-empty? segment)
   (eqv? (Segment-geom segment) 'e))

(: is-same-color? (-> Segment Segment Boolean))
(define (is-same-color? segment1 segment2)
  (eqv? (Segment-color segment1)
        (Segment-color segment2)))

(: is-open-and-peg? (-> Segment Segment Boolean))
(define (is-open-and-peg? segment1 segment2)
  (or (and (is-open? segment1) (is-peg? segment2))
      (and (is-open? segment2) (is-peg? segment1))))

(: can-cover-peg? (-> Segment Segment Boolean))
(define (can-cover-peg? segment1 segment2)
  (and (is-open-and-peg? segment1 segment2)
       (is-same-color? segment1 segment2)))

;place-which generates new segment to be placed
(: place-which? (-> Segment Segment Segment))
(define (place-which? board-segment piece-segment)
  (if (is-open-and-peg? board-segment piece-segment)
      (Segment (Segment-point board-segment)
               (Segment-color piece-segment)
               'c)
      (Segment (Segment-point board-segment)
               (Segment-color piece-segment)
               (Segment-geom piece-segment))))

(: update-board-segment (-> Segment Piece Segment))
(define (update-board-segment board-segment piece)
  (let ([fsegs : (Listof Segment) (filter (lambda ([piece-segment : Segment]) (point-eqv? (Segment-point piece-segment)
                                                                                          (Segment-point board-segment)))
                       (Piece-segments piece))])
    (if (empty? fsegs)
        board-segment
        (place-which? board-segment (first fsegs)))))

(: place-piece (-> Board Piece Board)) 
(define (place-piece board bpiece)
  (map (lambda ([board-segment : Segment])
         (update-board-segment board-segment
                               bpiece))
       board))

(: can-place-piece? (-> Board Piece Boolean))
(define (can-place-piece? board bpiece)
  (andmap (lambda ([segment : Segment])
            (and (in-bounds? (Segment-point segment))
                 (let ([board-segment : Segment (get-board (Segment-point segment) board)])
                   (or (is-empty? board-segment)
                       (can-cover-peg? board-segment
                                       segment)))))
          (Piece-segments bpiece)))

(: generate-all-moves (-> Board (Listof Piece) (Listof Piece)))
(define (generate-all-moves board pieces)
  (foldl (lambda ([piece : Piece][res : (Listof Piece)])
           (append res
                   (foldl (lambda ([board-segment : Segment][res2 : (Listof Piece)])
                            (let ([bpiece : Piece (update-piece-coords (Segment-point board-segment) piece board)])
                              (if (can-place-piece? board bpiece)
                                  (cons bpiece res2)
                                  res2)))
                          '()
                          board)))
           '()
           pieces))

(: moves (Listof Piece))
(define moves (generate-all-moves EMPTY-BOARD (generate-transformations PIECE-LIST)))

(: segment-to-string (-> Segment String))
(define (segment-to-string segment)
  (string-append (symbol->string (Segment-color segment))
                 (symbol->string (Segment-geom segment))))

(: get-row-n (-> Board Integer (Listof String)))
(define (get-row-n board n)
  (map (lambda ([segment : Segment])
         (segment-to-string segment))
       (filter
        (lambda ([segment : Segment])
          (equal? (Point-row (Segment-point segment)) n))
        board)))

(: board-to-string (-> Board (Listof (Listof String))))
(define (board-to-string board)
  (list
   (get-row-n board 0)
   (get-row-n board 1)
   (get-row-n board 2)
   (get-row-n board 3)))