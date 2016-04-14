(ns chess.core-test
  (:require [midje.sweet :refer :all]
            [chess.core :refer :all]))

(fact (count (legal-moves {:board starting-board})) => 20)

(fact (set (map :to-square (queen-squares starting-board [3 3]))) =>
         #{[2 2] [1 1] [2 3] [1 3] [2 4] [1 5] [3 2] [3 1]
           [3 0] [3 4] [3 5] [3 6] [3 7] [3 8] [4 2] [5 1] [6 0] [4 3]
           [5 3] [6 3] [4 4] [5 5] [6 6]})

(fact (set (unblocked-squares starting-board [3 3] [1 1])) =>
           #{[4 4] [5 5] [6 6]})

(fact (set (reachable-squares [3 3] [1 1])) =>
           #{[4 4] [5 5] [6 6] [7 7] [8 8]})

(fact (set (map :to-square (bishop-squares starting-board [3 3]))) =>
              #{[2 2] [1 1] [2 4] [1 5] [4 2] [5 1] [6 0] [4 4] [5 5] [6 6]})

(fact (set (map :to-square (rook-squares starting-board [3 3]))) =>
           #{[2 3] [1 3] [3 2] [3 1] [3 0] [3 4] [3 5]
             [3 6] [3 7] [3 8] [4 3] [5 3] [6 3]})

(rook-squares starting-board [7 0])

(fact (set (map :to-square (knight-squares [2 2]))) =>
           #{[0 1]
             [0 3]
             [1 0]
             [1 4]
             [3 0]
             [3 4]
             [4 1]
             [4 3]})

(fact (square-to-alg [0 0]) => "a8")

(fact (square-to-alg (alg-to-square "a8")) => "a8")
