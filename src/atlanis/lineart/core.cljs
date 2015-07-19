(ns atlanis.lineart.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom {:segments []
                          :bounds {:width 1000
                                   :height 500}}))
(defn add-segment! [segment]
  (swap! app-state update-in [:segments] conj segment))

(defn random-point []
  [(rand-int (get-in @app-state [:bounds :width]))
               (rand-int (get-in @app-state [:bounds :height]))])

;;; need better ways of choosing these
(defn random-angle
  []
  (* 2 Math/PI (rand)))

(defn random-dist []
  (+ (* 500 (rand)) 100))

(defn in-bounds? [[x y]]
  (and (< 0 x)
       (< 0 y)
       (< x (get-in @app-state [:bounds :width]))
       (< y (get-in @app-state [:bounds :height]))))

(defn within-bounds [[x y]]
  [(max (min x (get-in @app-state [:bounds :width])) 0)
   (max (min y (get-in @app-state [:bounds :height])) 0)])

(defn random-segment
  ([]
   [(random-point) (random-point)])
  ([prev]
   [prev (let [[px py] prev
               angle (random-angle)
               dist (random-dist)]
           (within-bounds
            [(+ px (* (Math/cos angle) dist))
             (+ py (* (Math/sin angle) dist))]))]))

(defn ccw [[ax ay]
           [bx by]
           [cx cy]]
  (> (* (- cy ay) (- bx ax))
     (* (- by ay) (- cx ax))))

(defn segments-intersect?
  "Predicate determining if two segments intersect. The eventual goal is to
  allow two segments to touch but not cross. This is an intermediary to that
  goal."
  [[a1 a2] [b1 b2]]
  (and (not= (ccw a1 b1 b2) (ccw a2 b1 b2))
       (not= (ccw a1 a2 b1) (ccw a1 b1 b2))))

(defn successor-segment [limit]
  (let [segments (:segments @app-state)]
    (loop [pos (dec (count segments))
           iter 0]
      (let [top (last (nth segments pos))
            segment (random-segment top)]
        (if (< iter limit)
          (if (not-any? (fn [other]
                          (segments-intersect? segment other))
                        segments)
            segment
            (recur pos (inc iter)))
          (recur (dec pos) 0))))))

(defn position [[[x1, y1], [x2, y2]]]
  {:x1 x1
   :y1 y1
   :x2 x2
   :y2 y2})

(defn norm [vec]
  (Math/sqrt
   (reduce #(+ %1 (Math/pow %2 2)) 0 vec)))

(defn dist [[first, second]]
  (norm (map - first second)))

(defn animation-time
  [dist]
  (/ dist 1000))

(defn segment [data owner]
  (reify
    om/IRender
    (render [this]
      (let [dist (dist data)]
        (dom/line (clj->js (merge
                            {:style {:stroke "black",
                                     :strokeWidth "2px"
                                     :strokeDasharray dist
                                     :strokeDashoffset dist
                                     :animation (apply str "dash " 1 "s linear forwards")}}
                            (position data))) nil)))))

(defn path [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/g nil
             (om/build-all segment (:segments data))))))

(defn main []
  (add-segment! (random-segment))
  (. js/window (setInterval #(add-segment! (successor-segment 1000)) 1000))
  (om/root path app-state
           {:target (. js/document (getElementById "container"))}))

(main)
