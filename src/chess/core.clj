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
       [(back-row :black)
        (second-row :black)
        empty-row
        empty-row
        empty-row
        empty-row
        (second-row :white)
        (back-row :white)]))

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

(def alg-to-square
  (let [letters (zipmap "abcdefgh" (range))
        numbers (zipmap "87654321" (range))]
    (fn
      [alg]
      (reverse (map deliver [letters numbers] alg)))))

(def square-to-alg
  (let [letters (zipmap (range) "abcdefgh")
        numbers (zipmap (range) "87654321")]
    (fn [square]
      (apply str (map deliver [letters numbers] square)))))

(defn king-squares
  [square]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (= 0 i) (= 0 j)))]
    (map + [i j] square)))

(defn knight-squares
  [square]
  (for [i [-2 -1 1 2]
        j [-2 -1 1 2]
        :when (odd? (+ i j))]
    (map + [i j] square)))

(defn reachable-squares
  [square direction]
  (for [n (range 1 8)
        :let [new-square (map + square (map #(* n %) direction))]
        :while (every? #(<= 0 % 8) new-square)]
    new-square))

(defn unblocked-squares
  [board square direction]
  (reduce (fn [moves position]
            (if (get-in board position)
              (reduced (conj moves position))
              (conj moves position)))
          []
          (reachable-squares square direction)))

(defn bishop-squares
  [board square]
  (for [i [-1 1]
        j [-1 1]
        sq (unblocked-squares board square [i j])]
    sq))

(defn rook-squares
  [board square]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (odd? (+ i j))
        sq (unblocked-squares board square [i j])]
    sq))

(defn queen-squares
  [board square]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (zero? i) (zero? j)))
        sq (unblocked-squares board square [i j])]
    sq))

(defn pawn-forward
  "Returns the list of forward square available to a pawn"
  [square color n]
  (map + square [(if (= :white color) (- n) n) 0]))

(defn pawn-capture-squares
  "Returns the list of forward square available to a pawn"
  [square color]
  (let [n (if (= :white color) -1 1)]
    [(map + square [n -1]) (map + square [n 1])]))

(def opposite {:white :black,
               :black :white})

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

(defn pawn-squares
  [board square]
  (let [{:keys [color]} (get-in board square)]
    (concat [(pawn-forward square color 1)]
            (if (starting-rank? color square)
              [(pawn-forward square color 2)])
            (pawn-captures board square))))

(defn moveable-squares
  [board square]
  (let [{:keys [piece color]} (get-in board square)]
    (cond
      (= :pawn piece) (pawn-squares board square)
      (= :knight piece) (knight-squares square)
      (= :king piece) (king-squares square)
      (= :biship piece) (bishop-squares board square)
      (= :rook piece) (rook-squares board square)
      (= :queen piece) (queen-squares board square))))

(defn move-piece
  [move board-state]
  (-> board-state
    (assoc-in (alg-to-square (:from move)) nil)
    (assoc-in (alg-to-square (:to move)) (:piece move))))

(defn legal?
  [move {:keys [board]}]
  (and (= (:piece move) (get-in board (alg-to-square (:from move))))))

(defn render
  [{:keys [board]}]
  (string/join \newline
    (for [row board]
      (string/join " " (map icons row)))))

(defn as-html [{:keys [board]}]
  [:table#board
   (cons [:tr.row (for [letter " ABCDEFGH"]
                    [:td letter])]
   (map (fn [row offset rank]
          [:tr.row
           (cons [:td rank]
                 (map (fn [piece color]
                        [:td.piece {:class color}
                         (icons piece)])
                      row offset))])

        board
        (iterate next (cycle ["white" "black"]))
        (range 8 0 -1)
        ))])

(defn test-endpoint
  [req]
  [:pre (render {:board (move-piece {:from  "e2"
                                     :to    "e4"
                                     :piece :P}
                                    (:board @game-state))})])

(defn legal-move?
  [board from to]
  (some #(= (alg-to-square to) %)
        (moveable-squares board (alg-to-square from))))

(defn move!
  [req]
  (:return
    (let [from (get (:params req) "from")
          to   (get (:params req) "to")]
      (swap! game-state
        (fn [state]
          (if (legal-move? (:board state) from to)
            (assoc
              (update state :board
                (fn [board]
                  (let [piece (get-in board (alg-to-square from))]
                    (-> board
                        (assoc-in (alg-to-square from) nil)
                        (assoc-in (alg-to-square to) piece)))))
              :return "OK")
            (assoc state :return "NO")))))))

(defroutes handler
  (GET "/" [] (html [:h1 "Hello World"]))
  (GET "/test" [req] (html (test-endpoint req)))
  (GET "/render" [req] (render @game-state))
  (GET "/render.html" [req] (html [:pre (render @game-state)]))
  (GET "/styled" [req] (html [:head (include-css "board.css")] [:body (as-html @game-state)]))
  (POST "/move" [] move!)
  (route/not-found (html [:h1 "Page not found"])))

(def app
  (-> handler
      wrap-params
      (wrap-resource "public")))
