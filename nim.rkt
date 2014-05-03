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


(define dumb-move
  (lambda nim-board
    (cond
      [(empty? nim-board) empty]
      [(zero? (first nim-board)) (cons (first nim-board) (apply dumb-move (rest nim-board)))]
      [else (cons (- (first nim-board) 1) (rest nim-board))])))


(dumb-move 1 2 3 4 5)


;;best-move nim-board -> nim-board

(define best-move
  (lambda nim-board
    (define B nim-board)
    (define X (apply nim-sum nim-board))
    (cond 
      [(zero? X) (apply dumb-move nim-board)]
      [else
       (define helper
         (lambda nim-board
           (cond
             [(empty? nim-board) empty]
             [else (let ([Y (nim-sum X (first nim-board))])
                     (cond 
                       [ (<= Y (first nim-board)) (cons Y (rest nim-board))]
                       [else (cons (first nim-board) (apply helper (rest nim-board)))]))])))
       (apply helper nim-board)])))


(best-move 9 6 5 4 3)
(best-move 7 6 5 4 3)





(define did-win?
  (lambda nim-board
    (foldr (lambda (x y) (and x y)) #t (map zero? nim-board))))


(define (user-move row num-take . nim-board)
  (cond
    [(empty? nim-board) (error "invalid row!")]
    [((compose not =) row 1) (cons (first nim-board) (apply user-move (- row 1) num-take (rest nim-board)))]
    [(< (first nim-board) num-take) (error "invalid number to take!")]
    [else (cons (- (first nim-board) num-take) (rest nim-board))]))

(did-win? 7 6 5 4)
(did-win? 0 0 0)


(define (turn-choice t user-fn computer-fn)
  (match t
    ['user (user-fn)]
    ['computer (computer-fn)]))



(define (play-nim turn . nim-board)
    (apply nim-board-print nim-board)
    (let ([B 
           (turn-choice turn (lambda () (let*
                                            ([row (begin (printf "Type the row to take from: ")
                                                  (read))]
                                             [num-take (begin (printf "Type the number of items to take from row ~a: " row)
                                                       (read))])
                                          (printf "~n")
                                          (apply user-move row num-take nim-board)))
                        (lambda () (apply best-move nim-board)))])
      
      (cond
        [(apply did-win? B)
         (apply nim-board-print nim-board)
         (turn-choice turn (lambda () (printf "Congratulations! You won!~n")) (lambda () (printf "Looks like I won!")))]
        [else
         (apply play-nim
                (turn-choice turn (lambda () 'computer) (lambda () 'user))
                B)])))
                       
                       



(play-nim 'user 5 4 3)

(play-nim 'computer 5 4 3)



;
;(define play-nim
;  (lambda nim-board
;    (apply nim-board-print nim-board)
;    (let*
;        ([user-row 
;          (begin
;            (printf "Type the row you wish to take from: ")
;            (read))]
;         [user-num-take 
;          (begin
;            (printf "Type the number of items you wish to take: ")
;            (read))]
;         
;         [new-board (apply user-move user-row user-num-take nim-board)])
;      (apply nim-board-print new-board)
;      (cond
;        [(apply did-win? new-board) (printf "Congratulations! You Won!")]
;        
;        [else
;         (let 
;             ([B (apply best-move new-board)])
;           
;           
;           (cond
;             [(apply did-win? B) 
;              (apply nim-board-print B)
;              (printf "Looks like I won!")]
;             [else (apply play-nim B)]))]))))
;
;
