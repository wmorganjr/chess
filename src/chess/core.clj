(ns chess.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-css]]
            [clojure.string :as string]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.params :refer [wrap-params]]))

(defn back-row
  [color]
  (for [piece [:rook :knight :bishop :queen :king :bishop :knight :rook]]
    {:piece piece :color color}))

(defn second-row
  [color]
  (repeat 8 {:piece :pawn :color color}))

(def empty-row
  (repeat 8 nil))

(def starting-board
  (mapv vec
       [(back-row :white)
        (second-row :white)
        empty-row
        empty-row
        empty-row
        empty-row
        (second-row :black)
        (back-row :black)]))

(def starting-player
  {:previous-move nil
   :has-castled false})

(def game-state
  (atom {:board starting-board
         :players {:white starting-player
                   :black starting-player}}))

(def icons
  {nil " "
   {:color :white :piece :king}   \u2654
   {:color :white :piece :queen}  \u2655
   {:color :white :piece :rook}   \u2656
   {:color :white :piece :bishop} \u2657
   {:color :white :piece :knight} \u2658
   {:color :white :piece :pawn}   \u2659
   {:color :black :piece :king}   \u265A
   {:color :black :piece :queen}  \u265B
   {:color :black :piece :rook}   \u265C
   {:color :black :piece :bishop} \u265D
   {:color :black :piece :knight} \u265E
   {:color :black :piece :pawn}   \u265F})

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
  [board piece indices]
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

(defn as-html [{:keys [board]}]
  [:table#board
   (map (fn [row offset]
          [:tr.row
           (map (fn [piece color]
                  [:td.piece {:class color}
                   (icons piece)])
                row offset)])
        board (iterate next (cycle ["white" "black"])))])

(defn test-endpoint
  [req]
  [:pre (render {:board (move-piece {:from  "e2"
                                     :to    "e4"
                                     :piece :P}
                                    (:board @game-state))})])

(defroutes handler
  (GET "/" [] (html [:h1 "Hello World"]))
  (GET "/test" [req] (html (test-endpoint req)))
  (GET "/render" [req] (render @game-state))
  (GET "/render.html" [req] (html [:pre (render @game-state)]))
  (GET "/styled" [req] (html [:head (include-css "board.css")] [:body (as-html @game-state)]))
  (route/not-found (html [:h1 "Page not found"])))

(def app
  (-> handler
      wrap-params
      (wrap-resource "public")))