(ns maze-clojure-remix.core
  (:gen-class))
(def size 20)

(defn last-one [rooms row col]
  (let [filtered-rooms (filter #(not (:visited? %)) (flatten rooms))
        lastrow (:row (first filtered-rooms))
        lastcol (:col (first filtered-rooms))]
    (if (= 1 (count filtered-rooms))
      (assoc-in rooms [lastrow lastcol :end?] true)
      rooms)))
      
(defn create-rooms []
  (vec 
    (for [row (range size)]
      (vec 
        (for [col (range size)]
          {:row row :col col :visited? false :bottom? true :right? true})))))

(defn possible-neighbors [rooms row col]
  (vec
    (remove 
      (fn [room]
        (or (nil? room) (:visited? room)))
      [(get-in rooms [(dec row) col])
       (get-in rooms [(inc row) col])
       (get-in rooms [row (dec col)])
       (get-in rooms [row (inc col)])])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))

(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ;going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ;going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ;going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ;going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(defn create-maze [rooms row col]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)
        rooms (last-one rooms row col)]
    (if next-room
        (loop [old-rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
          (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room))]
            (if (= old-rooms new-rooms)
              old-rooms
              (recur new-rooms))))
      rooms)))
        
(defn -main []
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0)
        rooms (assoc-in rooms [0 0 :start?] true)]
    ;print top walls
    (doseq [row rooms]
      (print " _"))
    (println)
    ;print grid
    (doseq [row rooms]
      (print "|");left wall
      (doseq [room row]
        (cond
          (:start? room)
          (print "O")
          (:end? room)
          (print "X")
          (:bottom? room)
          (print "_")
          :else
          (print " "))
        (if (:right? room)
          (print "|")
          (print " ")))
      (println))))
      
