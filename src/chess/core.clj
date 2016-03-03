(ns chess.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.string :as string]
            [ring.middleware.params :refer [wrap-params]]))

(defn back-row
  [color]
  (for [piece [:rook :knight :bishop :queen :king :bishop :knight :rook]]
    {:piece piece :color color}))

(defn second-row
  [color]
  (repeat 8 {:piece :pawn :color color}))

(def starting-board
  [[:r :n :b :q :k :b :n :r]
   [:p :p :p :p :p :p :p :p]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [:P :P :P :P :P :P :P :P]
   [:R :N :B :Q :K :B :N :R]])

(def starting-player
  {:previous-move nil
   :has-castled false})

(def game-state
  (atom {:board starting-board
         :players {:white starting-player
                   :black starting-player}}))

(def icons
  {nil " "
   :K \u2654
   :Q \u2655
   :R \u2656
   :B \u2657
   :N \u2658
   :P \u2659
   :k \u265A
   :q \u265B
   :r \u265C
   :b \u265D
   :n \u265E
   :p \u265F})

(def alg-to-indices
  (let [letters (zipmap "abcdefgh" (range))
        numbers (zipmap "87654321" (range))]
    (fn
      [alg]
      (reverse (map deliver [letters numbers] alg)))))

(def indices-to-alg
  (let [letters (zipmap (range) "abcdefgh")
        numbers (zipmap (range) "87654321")]
    (fn [indices]
      (apply str (map deliver [letters numbers] indices)))))

(assert (= "a8" (indices-to-alg [0 0])))
(assert (= "a8" (indices-to-alg (alg-to-indices "a8"))))

(defn adjacent-squares
  [indices]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (= 0 i) (= 0 j)))]
    (map + [i j] indices)))

(defn knight-squares
  [indices]
  (for [i [-2 -1 1 2]
        j [-2 -1 1 2]
        :when (odd? (+ i j))]
    (map + [i j] indices)))

(assert (= (set (knight-squares [2 2]))
           #{[0 1]
             [0 3]
             [1 0]
             [1 4]
             [3 0]
             [3 4]
             [4 1]
             [4 3]}))

(defn reachable-squares
  [indices direction]
  (for [n (range 1 8)
        :let [new-indices (map + indices (map #(* n %) direction))]
        :while (every? #(<= 0 % 8) new-indices)]
    new-indices))

(defn unblocked-squares
  [board indices direction]
  (reduce (fn [moves position]
            (if (get-in board position)
              (reduced (conj moves position))
              (conj moves position)))
          []
          (reachable-squares indices direction)))

(defn bishop-squares
  [board indices]
  (for [i [-1 1]
        j [-1 1]
        sq (unblocked-squares board indices [i j])]
    sq))

(defn rook-squares
  [board indices]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (odd? (+ i j))
        sq (unblocked-squares board indices [i j])]
    sq))

(defn queen-squares
  [board indices]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (zero? i) (zero? j)))
        sq (unblocked-squares board indices [i j])]
    sq))

(unblocked-squares starting-board [3 3] [1 1])
(reachable-squares [3 3] [1 1])
(bishop-squares starting-board [3 3])
(rook-squares starting-board [3 3])
(queen-squares starting-board [3 3])

(defn moveable-squares
  [piece indices]
  (cond
    (#{:k :K} piece) (adjacent-squares indices)
    (#{:n :N} piece) (knight-squares indices)
    (#{:b :B} piece) (bishop-squares board indices)
    (#{:r :R} piece) (rook-squares board indices)
    (#{:q :Q} piece) (queen-squares board indices)))

(defn move-piece
  [move board-state]
  (-> board-state
    (assoc-in (alg-to-indices (:from move)) nil)
    (assoc-in (alg-to-indices (:to move)) (:piece move))))

(defn legal?
  [move {:keys [board]}]
  (and (= (:piece move) (get-in board (alg-to-indices (:from move))))))

(defn render
  [{:keys [board]}]
  (string/join \newline
    (for [row board]
      (string/join " " (map icons row)))))

(defn test-endpoint
  [req]
  (str "<pre>"
  (render {:board (move-piece {:from  "e2"
                               :to    "e4"
                               :piece :P}
                              (:board @game-state))})
  "</pre>"))

(defroutes handler
  (GET "/" [] "<h1>Hello World</h1>")
  (GET "/render" [req] (render @game-state))
  (GET "/test" [req] (test-endpoint req))
  (GET "/render.html" [req] (str "<pre>" (render @game-state) "</pre>")) 
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> handler
      wrap-params))
