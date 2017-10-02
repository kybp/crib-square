(require-extension anaphora combinatorics format srfi-1 test)

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

(test-group "point-value"
  (let ((rank (lambda (r) (make-card r 'clubs))))
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
                (map (lambda (rank) (make-card rank suit)) +ranks+))
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

(test-group "make-tableau"
  (test "The tableau has 4 rows"
        4
        (length (tableau-rows (make-tableau))))
  (test "The tableau has 4 columns"
        4
        (length (tableau-columns (make-tableau))))
  (test "All spaces are initially #f"
        '(#f)
        (delete-duplicates (flatten (make-tableau)))))

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

(test-group "play-card"
  (let* ((card (make-card 2 'clubs))
         (tableau (play-card card 0 0 (make-tableau))))
    (test "After playing a card the tableau has 4 rows"
          4
          (length (tableau-rows tableau)))
    (test "After playing a card the tableau has 4 columns"
          4
          (length (tableau-columns tableau)))
    (test "Playing a card places that card into the tableau"
          (list '(#f #f #f #f)
                (list #f #f card #f)
                '(#f #f #f #f)
                '(#f #f #f #f))
          (play-card card 1 2 (make-tableau)))))

(define (n-copies n object)
  (do ((i 0 (+ i 1))
       (acc '() (cons object acc)))
      ((= i n) acc)))

(test-group "n-copies"
  (test "n = 0 returns the empty list" '() (n-copies 0 'blah))
  (test "n > 0 returns a list of n `object`s"
        '(blah blah blah)
        (n-copies 3 'blah)))

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

(test-group "find-runs"
  (test "Returns the empty list for a hand with no runs"
        '()
        (find-runs (list (make-card 2 'clubs)
                         (make-card 3 'clubs)
                         (make-card 5 'clubs)
                         (make-card 'king 'clubs))))
  (test "A run of 3 scores a (run 3)"
        '((run 3))
        (find-runs (list (make-card 3 'clubs)
                         (make-card 5 'clubs)
                         (make-card 9 'clubs)
                         (make-card 4 'hearts))))
  (test "A run of 4 scores a (run 4)"
        '((run 4))
        (find-runs (list (make-card 3 'clubs)
                         (make-card 4 'clubs)
                         (make-card 5 'clubs)
                         (make-card 6 'hearts))))
  (test "A run of 5 scores a (run 5)"
        '((run 5))
        (find-runs (list (make-card 10 'clubs)
                         (make-card 'king 'clubs)
                         (make-card 'jack 'clubs)
                         (make-card 9 'clubs)
                         (make-card 'queen 'clubs))))
  (test "Returns two 3-runs for a double run"
        '((run 3) (run 3))
        (find-runs (list (make-card 'ace 'clubs)
                         (make-card 2 'spades)
                         (make-card 3 'hearts)
                         (make-card 3 'diamonds))))
  (test "Returns three 3-runs for a triple run"
        (n-copies 3 '(run 3))
        (find-runs (list (make-card 'ace 'clubs)
                         (make-card 2 'spades)
                         (make-card 3 'hearts)
                         (make-card 3 'diamonds)
                         (make-card 3 'clubs))))
  (test "Returns two 4-runs for a double 4-run"
        '((run 4) (run 4))
        (find-runs (list (make-card 'ace 'clubs)
                         (make-card 2 'spades)
                         (make-card 3 'hearts)
                         (make-card 3 'diamonds)
                         (make-card 4 'spades)))))

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

(test-group "score-hand"
  (test "Four cards of the same suit scores a little-flush"
        '(little-flush)
        (score-hand (list (make-card 2 'clubs)
                          (make-card 4 'clubs)
                          (make-card 6 'clubs)
                          (make-card 8 'clubs))
                    (make-card 'king 'spades)))
  (test "Five cards of the same suit scores a big-flush"
        '(big-flush)
        (score-hand (list (make-card 2 'clubs)
                          (make-card 4 'clubs)
                          (make-card 6 'clubs)
                          (make-card 8 'clubs))
                    (make-card 'king 'clubs)))
  (test "Three cards of the same suit with a matching starter scores nothing"
        '()
        (score-hand (list (make-card 2 'clubs)
                          (make-card 4 'clubs)
                          (make-card 6 'clubs)
                          (make-card 8 'hearts))
                    (make-card 'king 'clubs)))
  (test "A run of 3 scores a (run 3)"
        '((run 3))
        (score-hand (list (make-card 8 'clubs)
                          (make-card 10 'clubs)
                          (make-card 9 'clubs)
                          (make-card 'ace 'hearts))
                    (make-card 2 'clubs)))
  (test "A run of 4 scores a (run 4)"
        '((run 4))
        (score-hand (list (make-card 'jack 'clubs)
                          (make-card 'king 'clubs)
                          (make-card 3 'clubs)
                          (make-card 10 'hearts))
                    (make-card 'queen 'clubs)))
  (test "A run of 5 scores a (run 5)"
        '((run 5))
        (score-hand (list (make-card 'jack 'clubs)
                          (make-card 9 'clubs)
                          (make-card 'queen 'clubs)
                          (make-card 10 'hearts))
                    (make-card 'king 'clubs)))
  (test "A pair scores a pair"
        '(pair)
        (score-hand (list (make-card 9 'clubs)
                          (make-card 9 'hearts)
                          (make-card 2 'spades)
                          (make-card 3 'hearts))
                    (make-card 7 'diamonds)))
  (test "Three of a kind scores 3 pairs"
        (n-copies 3 'pair)
        (score-hand (list (make-card 2 'clubs)
                          (make-card 2 'hearts)
                          (make-card 2 'spades)
                          (make-card 5 'diamonds))
                    (make-card 7 'spades)))
  (test "Four of a kind scores 6 pairs"
        (n-copies 6 'pair)
        (score-hand (list (make-card 2 'clubs)
                          (make-card 2 'hearts)
                          (make-card 2 'spades)
                          (make-card 2 'diamonds))
                    (make-card 8 'spades)))
  (test-assert "A double run scores two 3-runs and a pair"
               (let ((results (score-hand (list (make-card 'ace 'clubs)
                                                (make-card 2 'spades)
                                                (make-card 3 'hearts)
                                                (make-card 7 'diamonds))
                                          (make-card 'ace 'spades))))
                 (and (= 2 (count (lambda (x) (equal? x '(run 3))) results))
                      (= (length results) 3)
                      (memq 'pair results))))
  (test "Scores a 2-card fifteen"
        '(fifteen)
        (score-hand (list (make-card 5 'clubs)
                          (make-card 'king 'clubs)
                          (make-card 3 'spades)
                          (make-card 'ace 'spades))
                    (make-card 8 'hearts)))
  (test "Scores multiple fifteens"
        '(fifteen fifteen)
        (score-hand (list (make-card 'king 'clubs)
                          (make-card 7 'clubs)
                          (make-card 2 'clubs)
                          (make-card 'ace 'hearts))
                    (make-card 5 'diamonds))))

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

(test-group "hand-value"
  (test "Returns the sum of the hand's combination scores"
        16
        (hand-value (list (make-card 'king 'clubs)
                          (make-card 'queen 'clubs)
                          (make-card 'jack 'clubs)
                          (make-card 10 'clubs))
                    (make-card 5 'spades)))
  (test "Adds 1 to the combination score if the hand contains the jack"
        11
        (hand-value (list (make-card 'jack 'diamonds)
                          (make-card 'jack 'spades)
                          (make-card 'queen 'clubs)
                          (make-card 'king 'clubs))
                    (make-card 10 'diamonds))))

(define (game-value tableau starter)
  (apply + (map (lambda (hand) (hand-value hand starter))
                (tableau-hands tableau))))

(define (main)
  (let ((finish-game
         (lambda (tableau deck)
           (let ((starter (car (draw-card deck))))
             (format #t "You got ~a points.~%" (game-value tableau starter))
             (format #t "Explain? ")
             (unless (member (read) '(n no q exit))
                     (for-each (lambda (hand)
                                 (format #t "Hand: ~{~a~^ ~} (~a) - ~a points~%"
                                         (map card->string hand)
                                         (card->string starter)
                                         (hand-value hand starter))
                                 (when (jack-point? hand starter)
                                       (format #t "  jack - 1 point~%"))
                                 (for-each (lambda (c)
                                             (format #t "  ~a - ~a points~%"
                                                     c
                                                     (combination-value c)))
                                           (score-hand hand starter)))
                               (tableau-hands tableau)))))))
    (let loop ((tableau (make-tableau))
               (deck (make-deck))
               (turns 0))
      (print-tableau tableau)
      (if (= turns 16) (finish-game tableau deck)
          (let* ((result (draw-card deck))
                 (card (car result))
                 (deck (cdr result)))
            (format #t "You drew ~a~%" (card->string card))
            (format #t "Enter row then column number to place it: ")
            (let* ((row (read))
                   (column (read)))
              (loop (play-card card row column tableau)
                    deck
                    (+ turns 1))))))))
