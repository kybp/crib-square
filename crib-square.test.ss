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

(test-group "shuffle"
  (test "Shuffling a deck does not change the number of cards in it"
        52
        (length (shuffle (make-deck))))
  (test-assert "Shuffling a deck re-orders it"
               (let ((deck (make-deck)))
                 (not (equal? deck (shuffle deck))))))

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
  (test-assert "A run of 3 scores a 3-run"
               (let* ((three (make-card 3 'clubs))
                      (four (make-card 4 'hearts))
                      (five (make-card 5 'clubs))
                      (nine (make-card 9 'clubs))
                      (results (find-runs (list three five nine four))))
                 (equal? (list (list 'run three four five)) results)))
  (test-assert "A run of 4 scores a 4-run"
               (let* ((three (make-card 3 'clubs))
                      (four (make-card 4 'hearts))
                      (five (make-card 5 'clubs))
                      (six (make-card 6 'clubs))
                      (results (find-runs (list three five six four))))
                 (equal? (list (list 'run three four five six)) results)))
  (test-assert "A run of 5 scores a 5-run"
               (let* ((nine (make-card 9 'clubs))
                      (ten (make-card 10 'clubs))
                      (jack (make-card 'jack 'clubs))
                      (queen (make-card 'queen 'clubs))
                      (king (make-card 'king 'clubs))
                      (results (find-runs (list ten jack nine king queen))))
                 (equal? (list (list 'run nine ten jack queen king)) results)))
  (test-assert "Returns two 3-runs for a double run"
               (let* ((ace (make-card 'ace 'clubs))
                      (deuce (make-card 2 'spades))
                      (three-1 (make-card 3 'hearts))
                      (three-2 (make-card 3 'diamonds))
                      (results (find-runs (list ace deuce three-1 three-2))))
                 (and (= (length results) 2)
                      (member (list 'run ace deuce three-1) results equal?)
                      (member (list 'run ace deuce three-2) results equal?))))
  (test-assert "Returns three 3-runs for a triple run"
               (let* ((ace (make-card 'ace 'clubs))
                      (deuce (make-card 2 'spades))
                      (three-1 (make-card 3 'diamonds))
                      (three-2 (make-card 3 'hearts))
                      (three-3 (make-card 3 'spades))
                      (results
                       (find-runs (list ace deuce three-1 three-2 three-3))))
                 (and (= (length results) 3)
                      (member (list 'run ace deuce three-1) results equal?)
                      (member (list 'run ace deuce three-2) results equal?)
                      (member (list 'run ace deuce three-3) results equal?))))
  (test-assert "Returns two 4-runs for a double 4-run"
               (let* ((ace (make-card 'ace 'clubs))
                      (deuce (make-card 2 'spades))
                      (three-1 (make-card 3 'hearts))
                      (three-2 (make-card 3 'diamonds))
                      (four (make-card 4 'spades))
                      (results (find-runs (list ace deuce three-1 three-2 four))))
                 (and (= (length results) 2)
                      (member (list 'run ace deuce three-1 four) results equal?)
                      (member (list 'run ace deuce three-2 four) results equal?)))))

(define (scored-pair? card-1 card-2 result)
  (or (member (list 'pair card-1 card-2) result equal?)
      (member (list 'pair card-2 card-1) result equal?)))

(test-group "score-hand"
  (test "Four cards of the same suit scores a little-flush"
        '((little-flush clubs))
        (score-hand (list (make-card 2 'clubs)
                          (make-card 4 'clubs)
                          (make-card 6 'clubs)
                          (make-card 8 'clubs))
                    (make-card 'king 'spades)))
  (test "Five cards of the same suit scores a big-flush"
        '((big-flush clubs))
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
  (test-assert "A run of 3 scores a 3-run"
               (let* ((eight (make-card 8 'clubs))
                      (nine (make-card 9 'clubs))
                      (ten (make-card 10 'hearts))
                      (ace (make-card 'ace 'hearts))
                      (two (make-card 2 'clubs))
                      (results (score-hand (list eight ace ten nine) two)))
                 (equal? (list (list 'run eight nine ten)) results)))
  (test-assert "A run of 4 scores a 4-run"
               (let* ((ten (make-card 10 'hearts))
                      (jack (make-card 'jack 'clubs))
                      (queen (make-card 'queen 'clubs))
                      (king (make-card 'king 'hearts))
                      (three (make-card 3 'clubs))
                      (results (score-hand (list jack king three ten) queen)))
                 (equal? (list (list 'run ten jack queen king)) results)))
  (test-assert "A run of 5 scores a (run 5)"
               (let* ((ten (make-card 10 'hearts))
                      (jack (make-card 'jack 'clubs))
                      (queen (make-card 'queen 'clubs))
                      (king (make-card 'king 'hearts))
                      (nine (make-card 9 'clubs))
                      (results (score-hand (list jack nine queen ten) king)))
                 (equal? (list (list 'run nine ten jack queen king)) results)))
  (test-assert "A pair scores a pair"
        (let* ((nine-1 (make-card 9 'clubs))
               (nine-2 (make-card 9 'hearts))
               (results
                (score-hand
                 (list nine-1 nine-2 (make-card 2 'spades) (make-card 3 'hearts))
                 (make-card 7 'diamonds))))
          (scored-pair? nine-1 nine-2 results)))
  (test-assert "Three of a kind scores 3 pairs"
               (let* ((deuce-1 (make-card 2 'clubs))
                      (deuce-2 (make-card 2 'hearts))
                      (deuce-3 (make-card 2 'spades))
                      (hand (list deuce-1 deuce-2 deuce-3 (make-card 5 'diamonds)))
                      (result (score-hand hand (make-card 7 'spades))))
                 (and (= (length result) 3)
                      (scored-pair? deuce-1 deuce-2 result)
                      (scored-pair? deuce-1 deuce-3 result)
                      (scored-pair? deuce-2 deuce-3 result))))
  (test-assert "Four of a kind scores 6 pairs"
               (let* ((deuce-1 (make-card 2 'clubs))
                      (deuce-2 (make-card 2 'diamonds))
                      (deuce-3 (make-card 2 'hearts))
                      (deuce-4 (make-card 2 'spades))
                      (hand (list deuce-1 deuce-2 deuce-3 deuce-4))
                      (result (score-hand hand (make-card 8 'spades))))
                 (and (= (length result) 6)
                      (scored-pair? deuce-1 deuce-2 result)
                      (scored-pair? deuce-1 deuce-3 result)
                      (scored-pair? deuce-1 deuce-4 result)
                      (scored-pair? deuce-2 deuce-3 result)
                      (scored-pair? deuce-2 deuce-4 result)
                      (scored-pair? deuce-3 deuce-4 result))))
  (test-assert "A double run scores two 3-runs and a pair"
               (let* ((ace-1 (make-card 'ace 'clubs))
                      (ace-2 (make-card 'ace 'spades))
                      (results (score-hand (list ace-1
                                                 (make-card 2 'spades)
                                                 (make-card 3 'hearts)
                                                 (make-card 7 'diamonds))
                                           ace-2)))
                 (and (= 2 (count (lambda (x) (eq? (car x) 'run)) results))
                      (= (length results) 3)
                      (scored-pair? ace-1 ace-2 results))))
  (test "Scores a 2-card fifteen"
        (list (list 'fifteen
                    (make-card 5 'clubs)
                    (make-card 'king 'clubs)))
        (score-hand (list (make-card 5 'clubs)
                          (make-card 'king 'clubs)
                          (make-card 3 'spades)
                          (make-card 'ace 'spades))
                    (make-card 8 'hearts)))
  (test-assert "Scores multiple fifteens"
               (let* ((ace (make-card 'ace 'hearts))
                      (deuce (make-card 2 'clubs))
                      (seven (make-card 7 'clubs))
                      (king (make-card 'king 'clubs))
                      (five (make-card 5 'diamonds))
                      (result (score-hand (list king seven deuce ace)
                                          five)))
                 (and (= (length result) 2)
                      (member (list 'fifteen ace deuce five seven) result)
                      (member (list 'fifteen five king) result)))))

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
