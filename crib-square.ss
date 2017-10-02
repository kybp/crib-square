(require-extension anaphora combinatorics format srfi-1)

(define +suits+ '(clubs diamonds hearts spades))
(define +ranks+ '(king queen jack 10 9 8 7 6 5 4 3 2 ace))

(define (rank-index rank)
  (or (list-index (lambda (r) (eqv? r rank)) +ranks+)
      (error "rank-index: Invalid rank" rank)))

(define (rank-> rank-1 rank-2)
  (< (rank-index rank-1) (rank-index rank-2)))

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

(define (find-runs hand)
  (let ((ranks (sort (map card-rank hand) rank->))
        (consecutive? (lambda (last-rank rank)
                        (or (not last-rank)
                            (eqv? rank last-rank)
                            (= 1 (- (rank-index rank)
                                    (rank-index last-rank))))))
        (valid-run? (lambda (run)
                      (>= (length (delete-duplicates run)) 3))))
    (let loop ((remaining ranks)
               (current-run '())
               (best-run '())
               (last-rank #f))
      (when (> (length current-run) (length best-run))
            (set! best-run current-run))
      (cond ((and (null? remaining) (valid-run? best-run))
             (let ((pairs (- (length ranks)
                             (length (delete-duplicates ranks))))
                   (run-length (length (delete-duplicates best-run))))
               (n-copies (+ 1 pairs) (list 'run run-length))))
            ((null? remaining)
             '())
            (else (let ((rank (car remaining))
                        (ranks (cdr remaining)))
                    (loop ranks
                          (if (consecutive? last-rank rank)
                              (cons rank current-run)
                              (list rank))
                          best-run
                          rank)))))))

(define (find-pairs hand)
  (let ((groups '()))
    (for-each (lambda (rank)
                (aif (assoc rank groups eqv?)
                     (set-cdr! it (+ (cdr it) 1))
                     (set! groups (cons (cons rank 1) groups))))
              (map card-rank hand))
    (append-map (lambda (group)
                  (case (cdr group)
                    ((4) (n-copies 6 'pair))
                    ((3) (n-copies 3 'pair))
                    ((2) '(pair))
                    (else '())))
                groups)))

(define (find-fifteens hand)
  (let ((values (map point-value hand)))
    (do ((n-cards 2 (+ n-cards 1))
         (fifteens 0 (+ fifteens (combination-fold
                                  (lambda (numbers sum)
                                    (+ sum (if (= (apply + numbers) 15) 1 0)))
                                  0
                                  values
                                  n-cards))))
        ((> n-cards 5)
         (n-copies fifteens 'fifteen)))))

(define (find-flush hand starter)
  (let ((flush? (= 1 (length (delete-duplicates (map card-suit hand)))))
        (starter-matches-suit?
         (eq? (card-suit (car hand))
              (card-suit starter))))
    (cond ((and flush? starter-matches-suit?) '(big-flush))
          (flush? '(little-flush))
          (else '()))))

;; Score a list of 4 cards as a hand with the given starter and return a list
;; of symbols indicating won combinations.
(define (score-hand hand starter)
  (append (find-flush hand starter)
          (find-runs (cons starter hand))
          (find-pairs (cons starter hand))
          (find-fifteens (cons starter hand))))

(define (combination-value combination)
  (cond ((eq? combination 'fifteen) 2)
        ((eq? combination 'pair) 2)
        ((eq? combination 'little-flush) 4)
        ((eq? combination 'big-flush) 5)
        ((eq? (car combination) 'run) (cadr combination))))

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
