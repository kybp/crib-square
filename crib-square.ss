(require-extension anaphora combinatorics format srfi-1)

(define +suits+ '(clubs diamonds hearts spades))
(define +ranks+ '(king queen jack 10 9 8 7 6 5 4 3 2 ace))

(define (rank-index rank)
  (or (list-index (lambda (r) (eqv? r rank)) +ranks+)
      (error "rank-index: Invalid rank" rank)))

(define (rank-> rank-1 rank-2)
  (< (rank-index rank-1) (rank-index rank-2)))

(define (card-> card-1 card-2)
  (rank-> (card-rank card-1)
          (card-rank card-2)))

(define (card-< card-1 card-2)
  (not (or (card-> card-1 card-2)
           (equal? card-1 card-2))))

(define make-card cons)
(define card-rank car)
(define card-suit cdr)

(define (card->string card)
  (if (not card) "  "
      (let ((rank (card-rank card))
            (suit (card-suit card))
            (first-character (lambda (symbol)
                               (string-ref (symbol->string symbol) 0))))
        (format #f "~:(~a~a~)"
                (cond ((eqv? rank 10) #\T)
                      ((number? rank) rank)
                      (else (first-character rank)))
                (first-character suit)))))

(define (point-value card)
  (let ((rank (card-rank card)))
    (cond ((number? rank) rank)
          ((eq? rank 'ace) 1)
          (else 10))))

;; Return a new, unshuffled deck of 52 cards.
(define (make-deck)
  (append-map (lambda (suit)
                (map (lambda (rank) (make-card rank suit)) +ranks+))
              +suits+))

(define (shuffle deck)
  (let ((vector (list->vector deck)))
    (let loop ((n (- (vector-length vector) 1)) (acc '()))
      (if (negative? n) acc
          (let* ((i (random (+ n 1)))
                 (x (vector-ref vector i)))
            (vector-set! vector i (vector-ref vector n))
            (loop (- n 1) (cons x acc)))))))

;; Return a cons whose car is the card that was drawn and whose cdr is the
;; remaining cards in the deck.
(define draw-card identity)

(define (make-tableau)
  '((#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)
    (#f #f #f #f)))

(define (nth-row tableau n)
  (list-ref tableau n))

(define (nth-column tableau n)
  (map (lambda (row) (list-ref row n)) tableau))

(define tableau-rows identity)

(define (tableau-columns tableau)
  (list (nth-column tableau 0)
        (nth-column tableau 1)
        (nth-column tableau 2)
        (nth-column tableau 3)))

(define (tableau-hands tableau)
  (append (tableau-columns tableau)
          (tableau-rows tableau)))

(define (map-indexed function list)
  (let loop ((i 0) (list list) (acc '()))
    (if (null? list) (reverse acc)
        (loop (+ i 1)
              (cdr list)
              (cons (function i (car list)) acc)))))

(define (print-tableau tableau)
  (format #t "   ~{~3a~}~%" '(0 1 2 3))
  (format #t "~{~{~a~^ ~}~%~}"
          (map-indexed (lambda (i row)
                         (cons i (map card->string row)))
                       tableau)))

(define (get-card tableau row column)
  (list-ref column (list-ref row tableau)))

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

(define (n-copies n object)
  (do ((i 0 (+ i 1))
       (acc '() (cons object acc)))
      ((= i n) acc)))

(define (same-rank? card-1 card-2)
  (eqv? (card-rank card-1)
        (card-rank card-2)))

(define (spread-duplicates cards)
  (let ((run-length (length (delete-duplicates cards same-rank?))))
    (filter (lambda (run) (= (length run) run-length))
            (combination-map
             (lambda (run) (delete-duplicates run same-rank?))
             cards
             run-length))))

(define (find-runs hand)
  (let ((cards (sort hand card-<))
        (valid-run? (lambda ( run)
                      (>= (length (delete-duplicates run same-rank?)) 3)))
        (consecutive? (lambda (last-card card)
                        (and last-card
                             (let ((last-rank (card-rank last-card))
                                   (rank (card-rank card)))
                               (or (eqv? rank last-rank)
                                   (= 1 (- (rank-index last-rank)
                                           (rank-index rank)))))))))
    (let loop ((remaining cards)
               (current-run '())
               (best-run '())
               (last-card #f))
      (when (> (length current-run) (length best-run))
            (set! best-run current-run))
      (cond ((and (null? remaining) (valid-run? best-run))
             (let ((pairs (- (length cards)
                             (length (delete-duplicates cards same-rank?))))
                   (run-length (length (delete-duplicates best-run same-rank?))))
               (map (lambda (run) (cons 'run run)) (spread-duplicates best-run))))
            ((null? remaining)
             '())
            (else (let ((card (car remaining))
                        (cards (cdr remaining)))
                    (loop cards
                          (if (consecutive? last-card card)
                              (cons card current-run)
                              (list card))
                          best-run
                          card)))))))

(define (find-pairs hand)
  (combination-fold
   (lambda (cards acc)
     (if (eqv? (card-rank (car cards))
               (card-rank (cadr cards)))
         (cons (cons 'pair cards) acc)
         acc))
   '()
   hand
   2))

(define (find-fifteens hand)
  (do ((n-cards 2 (+ n-cards 1))
       (fifteens '() (combination-fold
                      (lambda (cards acc)
                        (if (= (apply + (map point-value cards)) 15)
                            (cons (cons 'fifteen (sort cards card-<)) acc)
                            acc))
                      fifteens
                      hand
                      n-cards)))
      ((> n-cards 5) fifteens)))

(define (find-flush hand starter)
  (let* ((suit (card-suit (car hand)))
         (flush? (= 1 (length (delete-duplicates (map card-suit hand)))))
         (starter-matches-suit? (eq? suit (card-suit starter))))
    (cond ((and flush? starter-matches-suit?)
           (list (list 'big-flush suit)))
          (flush?
           (list (list 'little-flush suit)))
          (else '()))))

;; Score a list of 4 cards as a hand with the given starter and return a list
;; of symbols indicating won combinations.
(define (score-hand hand starter)
  (append (find-flush hand starter)
          (find-runs (cons starter hand))
          (find-pairs (cons starter hand))
          (find-fifteens (cons starter hand))))

(define combination-type car)
(define combination-cards cdr)
(define combination-flush-suit cadr)

(define (combination-value combination)
  (case (combination-type combination)
    ((pair) 2)
    ((little-flush) 4)
    ((big-flush) 5)
    ((fifteen) 2)
    ((run) (length (combination-cards combination)))))

(define (jack-point? hand starter)
  (member (make-card 'jack (card-suit starter)) hand equal?))

(define (hand-value hand starter)
  (let ((combinations
         (apply + (map combination-value (score-hand hand starter)))))
    (if (jack-point? hand starter)
        (+ combinations 1)
        combinations)))

(define (game-value tableau starter)
  (apply + (map (lambda (hand) (hand-value hand starter))
                (tableau-hands tableau))))
