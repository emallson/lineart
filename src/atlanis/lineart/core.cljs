(ns atlanis.lineart.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom {:points []
                          :bounds {:width 1000
                                   :height 500}}))

(defonce point-chan (chan))

(defn random-point []
  [(rand-int (get-in @app-state [:bounds :width]))
   (rand-int (get-in @app-state [:bounds :height]))])

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

(defn norm [vec]
  (Math/sqrt
   (reduce #(+ %1 (Math/pow %2 2)) 0 vec)))

(defn dist [[first, second]]
  (norm (map - first second)))

(defn animation-time
  [pair]
  (/ (dist pair) 100))

(defn cross-2d
  [[vx vy] [wx wy]]
  (- (* vx wy)
     (* vy wx)))

(defn sub
  [a b]
  (map - a b))

(defn dot
  [a b]
  (reduce + (map * a b)))

(defn add
  [a b]
  (map + a b))

(defn scale
  [v s]
  (map (partial * s) v))

(defn segment-intersection
  "Computes the intersection of two segments or nil if there is none or infinitely many."
  [[p p2] [q q2]]
  (let [r (sub p2 p)
        s (sub q2 q)
        t (/ (cross-2d (sub q p) s)
             (cross-2d r s))
        u (/ (cross-2d (sub p q) r)
             (cross-2d s r))]
    (cond
      ;; parallel non-intersecting
      (and (zero? (cross-2d r s))
           (not (zero? (cross-2d (sub q p) r))))
      nil
      ;; co-linear
      (and (zero? (cross-2d r s))
           (zero? (cross-2d
                   (sub q p) r)))
      nil                               ; they may overlap, but are co-linear
                                        ; which is allowed
      ;; not co-linear
      (and (not (zero? (cross-2d r s)))
           (<= 0 t 1)
           (<= 0 u 1))
      (add p (scale r t))
      ;; all other cases
      :else nil)))

(defn segments-intersection
  "Given a line segment L, and a list of line segments compute the longest
  segment starting at L1 and no longer than L that does not intersect with any
  other segments."
  [segment others]
  (reduce (fn [segment other]
            (println segment)
            (if-let [intersection (segment-intersection segment other)]
              [(first segment) intersection]
              segment))
          segment others))

(defn generate-points-indefinitely
  ([]
   (let [point (random-point)]
     (put! point-chan point)
     (. js/window (setTimeout #(generate-points-indefinitely point) 0))))
  ([prev-point]
   (let [point (random-point)
         time (* (animation-time [prev-point point]) 1000)]
     (put! point-chan point)
     (. js/window (setTimeout #(generate-points-indefinitely point) time)))))

(defn position [[[x1, y1], [x2, y2]]]
  {:x1 x1
   :y1 y1
   :x2 x2
   :y2 y2})

(defn segment [data owner]
  (reify
    om/IRender
    (render [this]
      (let [{points :points,
             time :time} data]
        (dom/line (clj->js (merge
                            {:style {:stroke "black",
                                     :strokeWidth "2px"
                                     :strokeDasharray (dist points)
                                     :strokeDashoffset (dist points)
                                     :animation (apply str "dash " time "s linear forwards")}}
                            (position points))) nil)))))

(defn path [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:points []})
    om/IWillMount
    (will-mount [_]
      (let [point-chan (:point-chan data)]
        (go-loop []
          (let [points (om/get-state owner :points)
                others (partition 2 1 points)
                prev-point (last points)
                initial-point (<! point-chan)
                segment [prev-point initial-point]
                final-point (if (some (partial segments-intersect? segment) others)
                              (last (segments-intersection segment (drop-last others)))
                              initial-point)]
            (when-not (= prev-point final-point)
              (om/set-state! owner :points (conj points final-point)))
            (recur)))))
    om/IRenderState
    (render-state [this state]
      (let [pairs (partition 2 1 (:points state))]
        (dom/g nil
               (om/build-all segment (map (fn [pair] {:points pair,
                                                      :time (animation-time pair)})
                                          pairs)))))))

(defn main []
  (om/root path {:point-chan point-chan}
           {:target (. js/document (getElementById "container"))})
  (generate-points-indefinitely))

(main)
