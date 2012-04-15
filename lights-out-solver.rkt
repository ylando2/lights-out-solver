#lang slideshow

;;Programmer: Yuval Lando

(require racket/draw)

;;Set the value of the i X j position on the board.
(define (bset! board i j val)
  (vector-set! board (+ i (* j 5)) val))

;;Get the value of the i X j position on the board.
(define (bref board i j)
  (vector-ref board (+ i (* j 5))))

;;Get the x cordination of 2d array from the position on the flat vector.
(define (pos->x p)
   (remainder p 5))

;;Get the y cordination of 2d array from the position on the flat vector.
(define (pos->y p)
   (quotient p 5))

;;Find an open light that is above the last row. 
(define (find-top-row board i)
  (for/or ([i (in-range 0 (* 5 (- 5 1)))])
    (if (vector-ref board i)
        i
        #f)))

;;Turn a list to a more convenient vector for solving the puzzle. 
(define (build-board lst)
  (list->vector (map (lambda (x) (if (= x 1) #t #f)) lst)))

;;Toggle a light-ball on and off.
(define (turn board x y)
  (bset! board x y (not (bref board x y))))

;;Clicking on a position on the board.
(define (click board x y)
  (turn board x y)
  (when (> x 0)
    (turn board (- x 1) y))
  (when (< x 4)
    (turn board (+ x 1) y))
  (when (> y 0)
    (turn board x (- y 1)))
  (when (< y 4)
    (turn board x (+ y 1))))

;;A technique to solve the problem
(define (chase-light board)
  (let loop ([pos 0] [moves '()])
    (let ([newPos (find-top-row board pos)])
      (if newPos
        (let ((x (pos->x newPos)) (y (pos->y newPos))) 
          (click board x (+ y 1))
          (loop (+ 1 newPos) (cons (cons x (+ y 1)) moves) ))
        moves))))

;;Click on a possition above after we chase all the light down.
(define (alg-move board)
  (let ([last-row (vector-copy board 20 25)])
    (cond
      ((equal? last-row #(#t #f #f #f #t)) (click board 0 0) (click board 1 0) '((0 . 0) (1 . 0)) )
      ((equal? last-row #(#f #t #f #t #f)) (click board 0 0) (click board 3 0) '((0 . 0) (3 . 0)) )
      ((equal? last-row #(#t #t #t #f #f)) (click board 1 0) '((1 . 0)) )
      ((equal? last-row #(#f #f #t #t #t)) (click board 3 0) '((3 . 0)) )      
      ((equal? last-row #(#t #f #t #t #f)) (click board 4 0) '((4 . 0)) )      
      ((equal? last-row #(#f #t #t #f #t)) (click board 0 0) '((0 . 0)) )    
      ((equal? last-row #(#t #t #f #t #t)) (click board 2 0) '((2 . 0)) )
      ((equal? last-row #(#f #f #f #f #f)) '() )
      (else '() #f #;|Unsolvable| ))))

;;Solve the puzzle and return a list of pairs;
;;each pair (x . y) represent the cordination of the solution.
(define (solve board-lst)
  (let* ([board (build-board board-lst)]
         [sol1 (chase-light board)]
         [sol2 (alg-move board)])
    (if (not sol2)
        #f
        (let*
            ([sol3 (chase-light board)]
             [solution (append sol1 sol2 sol3)]
             [hash (make-hash)])
          (for-each 
           (lambda (e)
             (hash-set! hash e (+ (hash-ref hash e 0) 1)))             
           solution)
          (map (lambda (x) (car x)) 
               (filter (lambda (x) (= 1 (remainder (cdr x) 2)))  
                       (hash->list hash)))))))

;Debug function
;(define (print-solution solution)
;  (printf "the solution is: ~%")
;  (let ([i 0])
;    (for-each 
;     (lambda (e)
;       (printf " [ ~a , ~a ] " (+ (car e) 1) (+ (cdr e) 1))
;       (set! i (+ i 1))
;       (when (= 0 (remainder i 5))
;         (printf "~%"))) 
;     (sort solution (lambda (x y) (if (= (cdr x) (cdr y)) 
;                                      (< (car x) (car y))
;                                      (< (cdr x) (cdr y)))))))
;  (void))

(define *board* 
             '( 1 0 0 0 1
                1 1 0 1 1
                0 0 1 0 0
                1 0 1 0 0
                1 0 1 1 0 ))

;;;;;;;;;;; Helper drawing functions ;;;;;;;;;;;;;;;;;;;; 
(define (square size)
  (filled-rectangle size size))

(define (white-square size)
  (frame (colorize (square size) "white") #:color "black"))

(define (black-square size)
  (frame (colorize (square size) "black") #:color "black"))

(define (yellow-square size)
  (frame (colorize (square size) "yellow") #:color "black"))

(define (draw-char c size)
  (let* ([text-target (make-bitmap size size)]
         [dc (new bitmap-dc% [bitmap text-target])])
    (send dc set-font (make-object font% (- size 8) 'roman 'normal))
    (send dc draw-text (string c) 8 0)
    (bitmap text-target)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Put the solution into a vector that represent the board
;;0 - light off.
;;1 - light on.
;;'click - a square that is a part of the solution. 
(define (board-with-solution board solution)
  (let ([vec (list->vector board)])
    (for-each 
     (lambda (c) 
       (let ([x (car c)]
             [y (cdr c)])
         (bset! vec x y 'click)))
     solution)
    vec))

;;Turn a number 0-9 into a character #\0-#\9
(define (integer->digit n)
  (integer->char (+ n (char->integer #\0))))

;;Draw the puzzle
(define (draw-puzzle board)
  (let* ([vec (list->vector board)]
         [top
          (apply hc-append (cons 
                            (blank 20 20)
                            (build-list 
                             5 
                             (lambda (i) 
                               (draw-char (integer->digit (+ i 1)) 20)))))]
         [body
          (for/list ([j (in-range 5)])
            (apply hc-append
                   (cons
                    (draw-char (integer->digit (+ j 1)) 20)
                    (for/list ([i (in-range 5)])
                      (case (bref vec i j);(vector-ref vec (+ i (* j 5)))
                        [(0) (black-square 20)]
                        [(1) (white-square 20)])))))])
    (apply vc-append (cons top body))))

;;Draw the solution where you need to click on a yellow squares to solve the puzzle.
(define (draw-solution board)
  (let ([s (solve board)])
    (if s
        (let* ([vec (board-with-solution board s)]
               [top
                (apply hc-append (cons 
                                  (blank 20 20)
                                  (build-list 
                                   5 
                                   (lambda (i) 
                                     (draw-char (integer->digit (+ i 1)) 20)))))]
               [body
                (for/list ([j (in-range 5)])
                  (apply hc-append
                         (cons
                          (draw-char (integer->digit (+ j 1)) 20)
                          (for/list ([i (in-range 5)])
                            (case (bref vec i j)
                              [(0) (black-square 20)]
                              [(1) (white-square 20)]
                              [(click) (yellow-square 20)])))))])
          (apply vc-append (cons top body)))
        (text "Unsolvable puzzle" (current-main-font) 20))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
(draw-puzzle *board*)
(blank 10 10)
(draw-solution *board*)