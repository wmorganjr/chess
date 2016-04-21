(ns chess.move)

;; Accessors

(defn captured-square [move]
  (:capture move))

(defn promoted-piece [move]
  (:promotion move))

(defn from-square [move]
  (:square move))

(defn to-square [move]
  (:to move))

(defn moved-piece [move]
  (:piece move))

(defn castled-rook [move]
  (:castle-with move))

(defn notes [move]
  (:notes move))

;; Constructors

(defn move [piece from-square to-square]
  {:piece piece, :from from-square, :to to-square})

(defn promote [move piece]
  (assoc move :promotion piece))

(defn capture
  ([move]
     (capture move (to-square move)))
  ([move square]
     (assoc move :capture square)))

(defn notate
  [move note]
  (update move :notes (fnil conj []) note))

(defn castle [rook-square]
  {:castle-with rook-square})
