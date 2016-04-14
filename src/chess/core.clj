(ns chess.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-css]]
            [clojure.string :as string]
            [chess.pawn :refer [pawn-squares pawn-moves]]
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

(def game-state
  (atom {:board starting-board
         :moves (list)}))

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
    {:to-square (map + [i j] square)}))

(defn find-piece
  [board piece]
  (first (for [rank (range 8)
               file (range 8)
               :when (= piece (get-in board [rank file]))]
           [rank file])))

(defn knight-squares
  [square]
  (for [i [-2 -1 1 2]
        j [-2 -1 1 2]
        :when (odd? (+ i j))]
    {:to-square (map + [i j] square)}))

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
    {:to-square sq}))

(defn rook-squares
  [board square]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (odd? (+ i j))
        sq (unblocked-squares board square [i j])]
    {:to-square sq}))

(defn queen-squares
  [board square]
  (for [i [-1 0 1]
        j [-1 0 1]
        :when (not (and (zero? i) (zero? j)))
        sq (unblocked-squares board square [i j])]
    {:to-square sq}))

(defn moveable-squares
  [{:keys [board moves]} square]
  (let [{:keys [piece color]} (get-in board square)]
    (cond
      (= :pawn piece) (pawn-moves board moves square)
      (= :knight piece) (knight-squares square)
      (= :king piece) (king-squares square)
      (= :bishop piece) (bishop-squares board square)
      (= :rook piece) (rook-squares board square)
      (= :queen piece) (queen-squares board square))))

(def opposite {:white :black :black :white})

(defn turn
  [moves]
  (or (opposite (get-in (first moves) [:piece :color])) :white))

(defn update-game-state
  [{:keys [board] :as state} {:keys [from-square to-square piece]} capture]
  (-> state
    (update :board (fn [board]
                     (-> (if capture (assoc-in board capture nil) board)
                         (assoc-in from-square nil)
                         (assoc-in to-square piece))))
    (update :moves conj {:from-square from-square
                         :to-square   to-square
                         :piece       piece})))

(declare check?)

(defn legal?
  [{:keys [board moves] :as state} from-square to-square]
  (and (= (:color (get-in board from-square))
          (turn moves))

       (<= 0 (first to-square) 7)
       (<= 0 (second to-square) 7)

       (not= (:color (get-in board from-square))
             (:color (get-in board to-square)))))

(defn legal-move
  [{:keys [board moves] :as state} from-square to-square]
  (and (legal? state from-square to-square)
       (first (filter #(= to-square (:to-square %))
                      (moveable-squares state from-square)))))

(defn legal-moves
  [{:keys [board moves] :as state}]
  (let [color (turn moves)]
    (for [rank (range 8)
          file (range 8)
          :when (= color (get-in board [rank file :color]))
          move (moveable-squares state [rank file])
          :when (legal? state [rank file] (:to-square move))]
      (assoc move
             :from-square [rank file]))))

(defn null-move
  [color]
  {:piece {:color color}})

(defn check?
  [{:keys [board moves] :as state}]
  (let [color (turn moves)
        king  (find-piece board {:piece :king
                                 :color color})]
    (some #(= king (:to-square %))
          (legal-moves {:board board
                        :moves (conj moves (null-move color))}))))

(defn checkmate?
  [{:keys [board moves] :as state}]
  (and (check? state)
       (every? (fn [{:keys [from-square move capture] :as m}]
                 (check? (update (update-game-state state
                                                    {:from-square from-square
                                                     :to-square   move
                                                     :piece       (get-in board from-square)}
                                                    capture)
                                 :moves conj (null-move (opposite (turn moves))))))
               (legal-moves state))))

(defn do-move!
  [from-square to-square]
  (swap! game-state
         (fn [{:keys [board] :as state}]
           (let [piece (get-in board from-square)]
             (if-let [{:keys [capture]} (legal-move state from-square to-square)]
               (let [color (:color (get-in board from-square))
                     new-state (update-game-state state
                                         {:from-square from-square
                                          :to-square   to-square
                                          :piece       piece}
                                         capture)]
                 (if (check? (update new-state :moves conj (null-move (opposite color))))
                   (assoc state :return "NO")
                   (assoc new-state :return "OK")))
               (assoc state :return "NO"))))))

(defn move!
  [req]
  (:return
    (let [from (get (:params req) "from")
          from-square (alg-to-square from)
          to   (get (:params req) "to")
          to-square (alg-to-square to)]
      (do-move! from-square to-square))))

(defn random!
  [req]
  (when-let [{:keys [from-square move]} (rand-nth (legal-moves @game-state))]
    (do-move! from-square move)))

(defn debug!
  [req]
  (binding [legal-move (constantly true)]
    (move! req)))

(defn as-html [{:keys [board moves] :as state}]
  [:div [:table#board
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
              (range 8 0 -1)))]
        [:p (format "%s to play" (name (turn moves)))]
        [:p (cond (checkmate? state) "Checkmate!"
                  (check? state) "Check!"
                  :else "")]])

(defroutes handler
  (GET "/styled" [req] (html [:head (include-css "board.css")] [:body (as-html @game-state)]))
  (POST "/move" [] move!)
  (POST "/debug" [] debug!)
  (POST "/random" [] random!)
  (route/not-found (html [:h1 "Page not found"])))

(def app
  (-> handler
      wrap-params
      (wrap-resource "public")))
