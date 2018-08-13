#lang racket

(require rackunit)

(check-equal? 1 1)

(test-case
 "moves"
 (check-equal? 2 1))