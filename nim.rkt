#lang racket


(define (print-num num)
  (cond
    [(zero? num) (printf "~n")]
    [else (printf "*")
          (print-num (- num 1))]))

(define print-nim-board
  (lambda (len . nim-board)
    (cond
      ([empty? nim-board] (printf "~n"))
      (else 
       (printf "~a: " len)
       (print-num (first nim-board))
       (apply print-nim-board (+ len 1) (rest nim-board))))))

(print-nim-board 1 7 6 5 4 3)

(define nim-board-print
  (lambda nim-board
    (apply print-nim-board 1 nim-board)))



(nim-board-print 1 2 3 4 5)
(nim-board-print 5 4 3)

(define nim-sum bitwise-xor)


(define dumb-move-as-move
  (lambda (row . nim-board)
    (cond
      [(empty? nim-board) empty]
      [(zero? (first nim-board)) (apply dumb-move-as-move (+ row 1) (rest nim-board))]
      [else (values row 1)])))


(dumb-move-as-move 1 2 3 4 5)



(define best-move-as-move
  (lambda nim-board
    (define X (apply nim-sum nim-board))
    (cond
      [(zero? X) (apply dumb-move-as-move 1 nim-board)]
      [else
       (define (helper row . nim-board)
         (cond
           [(empty? nim-board) empty]
           [else (let ([Y (nim-sum X (first nim-board))])
                   (cond
                     [(<= Y (first nim-board)) (values row (- (first nim-board) Y))]
                     [else (apply helper (+ row 1) (rest nim-board))]))]))
       
       (apply helper 1 nim-board)])))

(best-move-as-move 9 6 5 4 3)



(define did-win?
  (lambda nim-board
    (foldr (lambda (x y) (and x y)) #t (map zero? nim-board))))


(define (move row num-take . nim-board)
  (cond
    [(empty? nim-board) (error "invalid row!")]
    [((compose not =) row 1) (cons (first nim-board) (apply move (- row 1) num-take (rest nim-board)))]
    [(< (first nim-board) num-take) (error "invalid number to take!")]
    [else (cons (- (first nim-board) num-take) (rest nim-board))]))

(did-win? 7 6 5 4)
(did-win? 0 0 0)


(define (turn-choice t user-fn computer-fn)
  (match t
    ['user (user-fn)]
    ['computer (computer-fn)]))


(define (get-user-move)
  (let*
      ([row (begin
              (printf "Type the row to take from:")
              (read))]
       [num-take
        (begin
          (printf "~nType the number of items to take from row ~a: " row)
          (read))])
    (printf "~n")
    (values row num-take)))


(define (play-nim turn . nim-board)
  (apply nim-board-print nim-board)
  (define-values (row num-take) (turn-choice turn
                                             get-user-move
                                             (thunk (apply best-move-as-move nim-board))))
  (define B (apply move row num-take nim-board))
  
  (cond
    [(apply did-win? B)
     (apply nim-board-print nim-board)
     (turn-choice turn (thunk (printf "Congratulations! You won!~n")) (thunk (printf "Looks like I won!~n")))]
    [else
     (apply play-nim
            (turn-choice turn (thunk 'computer) (thunk 'user))
            B)]))





(play-nim 'user 5 4 3)

(play-nim 'computer 5 4 3)



