(ns chess.move)

(defn move
  [piece from-square to-square])

;; Constructors

(defn promote
  [move piece])

(defn capture
  ([move])
  ([move square]))

(defn notate
  [move note])

(defn castle
  [rook-square])

;; Accessors

(defn captured-square
  [move])

(defn promoted-piece
  [move])

(defn from-square
  [move])

(defn to-square
  [move])

(defn moved-piece
  [move])

(defn castled-rook
  [move])

(defn notes
  [move])









