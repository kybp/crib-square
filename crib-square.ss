(require-extension format srfi-1 test)

(define +suits+ '(clubs diamonds hearts spades))
(define +ranks+ '(king queen jack ten 9 8 7 6 5 4 3 2 ace))

(define make-card cons)
(define card-suit car)
(define card-rank cdr)

(define (point-value card)
  (let ((rank (card-rank card)))
    (cond ((number? rank) rank)
          ((eq? rank 'ace) 1)
          (else 10))))

(test-group "point-value"
  (let ((rank (lambda (r) (make-card 'clubs r))))
    (test "Kings have a point value of 10" 10 (point-value (rank 'king)))
    (test "Queens have a point value of 10" 10 (point-value (rank 'queen)))
    (test "Jacks have a point value of 10" 10 (point-value (rank 'jack)))
    (test "Aces have a point value of 1" 1 (point-value (rank 'ace)))
    (do ((i 2 (+ i 1)))
        ((= i 10))
      (test (format #f "~as have a point value of ~:*~a" i)
            i
            (point-value (rank i))))))

;; Return a new, unshuffled deck of 52 cards.
(define (make-deck)
  (append-map (lambda (suit)
                (map (lambda (rank) (make-card suit rank)) +ranks+))
              +suits+))

(define deck-length length)
(define map-deck map)

(test-group "make-deck"
  (test "A deck initially has 52 cards" 52 (deck-length (make-deck)))
  (test "A deck has cards of 4 suits"
        4
        (length (delete-duplicates (map-deck card-suit (make-deck)))))
  (for-each (lambda (suit)
              (test (format #f "A deck initially has 13 ~a" suit)
                    13
                    (count (lambda (other-suit) (eq? suit other-suit))
                           (map-deck card-suit (make-deck)))))
            +suits+))

;; Return a cons whose car is the card that was drawn and whose cdr is the
;; remaining cards in the deck.
(define draw-card identity)

(test-group "draw-card"
  (test "Drawing a card decrements a deck's length by 1"
        (- (length (make-deck)) 1)
        (length (cdr (draw-card (make-deck)))))
  (test-assert "The drawn card was a member of the original deck"
               (let* ((original-deck (make-deck))
                      (card (car (draw-card original-deck))))
                 (member card original-deck)))
  (test-assert "The drawn card is not a member of the remaining deck"
               (let* ((result (draw-card (make-deck)))
                      (card (car result))
                      (deck (cdr result)))
                 (not (member card deck equal?)))))

(define (make-crib-squares)
  '((#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)))

(define (nth-row tableau n)
  (list-ref tableau n))

(define (nth-column tableau n)
  (map (lambda (row) (list-ref row n)) tableau))

(define crib-squares-rows identity)

(define (crib-squares-columns tableau)
  (list (nth-column tableau 0)
        (nth-column tableau 1)
        (nth-column tableau 2)
        (nth-column tableau 3)))

(test-group "make-crib-squares"
  (test "The tableau has 4 rows"
        4
        (length (crib-squares-rows (make-crib-squares))))
  (test "The tableau has 4 columns"
        4
        (length (crib-squares-columns (make-crib-squares))))
  (test "All spaces are initially #f"
        '(#f)
        (delete-duplicates (flatten (make-crib-squares)))))

(define (map-indexed function list)
  (let loop ((i 0) (list list) (acc '()))
    (if (null? list) (reverse acc)
        (loop (+ i 1)
              (cdr list)
              (cons (function i (car list)) acc)))))

;; Return a new tableau consisting of the original tableau with the given card
;; placed at the indicated, 0-indexed row and column intersection.
(define (play-card card row column tableau)
  (map-indexed
   (lambda (row-index old-row)
     (map-indexed
      (lambda (column-index old-card)
        (if (and (= row-index row)
                 (= column-index column))
            card
            old-card))
      old-row))
   tableau))

(test-group "play-card"
  (let* ((card (make-card 'clubs 2))
         (tableau (play-card card 0 0 (make-crib-squares))))
    (test "After playing a card the tableau has 4 rows"
          4
          (length (crib-squares-rows tableau)))
    (test "After playing a card the tableau has 4 columns"
          4
          (length (crib-squares-columns tableau)))
    (test "Playing a card places that card into the tableau"
          (list '(#f #f #f #f)
                (list #f #f card #f)
                '(#f #f #f #f)
                '(#f #f #f #f))
          (play-card card 1 2 (make-crib-squares)))))
