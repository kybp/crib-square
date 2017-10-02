(require-extension format srfi-1 test)
(load "crib-square.ss")

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

(test-group "make-deck"
  (test "A deck initially has 52 cards" 52 (length (make-deck)))
  (test "A deck has cards of 4 suits"
        4
        (length (delete-duplicates (map card-suit (make-deck)))))
  (for-each (lambda (suit)
              (test (format #f "A deck initially has 13 ~a" suit)
                    13
                    (count (lambda (other-suit) (eq? suit other-suit))
                           (map card-suit (make-deck)))))
            +suits+))

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

(test-group "n-copies"
  (test "n = 0 returns the empty list" '() (n-copies 0 'blah))
  (test "n > 0 returns a list of n `object`s"
        '(blah blah blah)
        (n-copies 3 'blah)))

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

(test-exit)
