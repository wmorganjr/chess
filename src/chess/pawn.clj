(ns chess.pawn
  (:require [chess.move :as move]))

(defn pawn-forward
  "Returns the list of forward square available to a pawn"
  [square color n]
  (map + square [(if (= :white color) (- n) n) 0]))

(defn pawn-capture-squares
  "Returns the list of forward square available to a pawn"
  [square color]
  (let [n (if (= :white color) -1 1)]
    [(map + square [n -1]) (map + square [n 1])]))

(def opposite {:white :black :black :white})

(defn pawn-captures
  [board square]
  (let [{:keys [color]} (get-in board square)]
    (filter #(= (opposite color)
                (:color (get-in board %)))
            (pawn-capture-squares square color))))

(defn starting-rank?
  [color [rank file]]
  (or (and (= :white color) (= rank 6))
      (and (= :black color) (= rank 1))))

(defn pawn-enpassants
  [board moves square]
  (let [{:keys [piece from-square to-square]} (first moves)
        [from-rank from-file] from-square
        [to-rank to-file] to-square]
    (if (and (= :pawn (:piece piece))
             (even? (+ from-rank to-rank)))
      (filter
        (fn [capture-square]
          (= capture-square
             (map + to-square [(if (= :white (:color piece))
                                 1 -1) 0])))
              (pawn-capture-squares square (:color (get-in board square)))))))

(defn pawn-squares
  [board moves square]
  (let [{:keys [color]} (get-in board square)]
    (concat [(pawn-forward square color 1)]
            (if (starting-rank? color square)
              [(pawn-forward square color 2)])
            (pawn-captures board square)
            (pawn-enpassants board moves square))))

(defn two-square-advance?
  [board color square]
  (and (starting-rank? color square)
       (nil? (get-in board (map + square [(if (= :white color) -1 1) 0])))))

(defn possible-moves
  [board moves square]
  (let [{:keys [color] :as pawn} (get-in board square)]
    (for [[target capture] (concat [[(pawn-forward square color 1), nil]]
                                   (if (two-square-advance? board color square)
                                     [[(pawn-forward square color 2), nil]])
                                   (for [square (pawn-captures board square)]
                                     [square, square])
                                   (for [square (pawn-enpassants board moves square)]
                                     [square, (map + square [(if (= :white color) 1 -1) 0])]))]
      (-> (move/move pawn target)
          (cond-> capture (move/capture capture))))))

(defn pawn-moves
  [board moves square]
  (->> (possible-moves board moves square)
       (filter (fn [move]
                 (or (move/captured-square move)
                     (nil? (get-in board (move/to-square move))))))))
