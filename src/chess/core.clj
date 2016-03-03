(ns chess.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
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

(def game-state
  (atom {:board starting-board}))

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

(defn render
  [{:keys [board]}]
  (string/join \newline
    (for [row board]
      (string/join " " (map icons row)))))

(defroutes handler
  (GET "/" [] "<h1>Hello World</h1>")
  (GET "/render" [req] (render @game-state)) 
  (GET "/render.html" [req] (str "<pre>" (render @game-state) "</pre>")) 
  (route/not-found "<h1>Page not found</h1>"))

(def app
  (-> handler
      wrap-params))
