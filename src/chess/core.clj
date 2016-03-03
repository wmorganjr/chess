(ns chess.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [clojure.string :as string]
            [ring.middleware.params :refer [wrap-params]]))

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

(defn move-piece
  [move board-state]
  (-> board-state
    (assoc-in (alg-to-indices (:from move)) nil)
    (assoc-in (alg-to-indices (:to move)) (:piece move))))

(defn render
  [{:keys [board]}]
  (string/join \newline
    (for [row board]
      (string/join " " (map icons row)))))

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
  (route/not-found (html [:h1 "Page not found"])))

(def app
  (-> handler
      wrap-params))
