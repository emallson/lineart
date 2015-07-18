(ns atlanis.lineart.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om]
            [om.dom :as dom]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom {:points []
                          :bounds {:width 1000
                                   :height 1000}}))

(defn random-point []
  [(rand-int (get-in @app-state [:bounds :width]))
   (rand-int (get-in @app-state [:bounds :height]))])

(swap! app-state update-in [:points] conj (random-point))
(swap! app-state update-in [:points] conj (random-point))

(defonce points-updated-chan (chan))

(defn join [parts inter]
  (loop [s (str (first parts))
         r (rest parts)]
    (if (empty? r)
      s
      (recur (str s inter (first r))
             (rest r)))))

(defn line-path
  "Compute the SVG path (d attribute) from a list of points."
  [points]
  (if (empty? points)
    ""
    (loop [path (str "M" (join (first points) ","))
           remaining (rest points)]
      (if (empty? remaining)
        path
        (let [curr (first remaining)
              next-path (str path "L" (join curr ","))]
          (recur next-path
                 (rest remaining)))))))

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
      (let [dist (dist data)
            time (animation-time dist)]
        (dom/line (clj->js (merge
                            {:style {:stroke "black",
                                     :strokeWidth "2px"
                                     :strokeDasharray dist
                                     :strokeDashoffset dist
                                     :animation (apply str "dash " time "s linear forwards")}}
                            (position data))) nil)))))


(defn add-point [chan]
  (put! chan (random-point)))

(defn path [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:point-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [point-chan (om/get-state owner :point-chan)]
        (add-point point-chan)
        (go-loop [point (<! point-chan)]
          (om/transact! data :points #(conj % point))
          (. js/window (setTimeout #(add-point point-chan) (animation-time (dist (take-last 2 (:points data)))))))))
    om/IRender
    (render [this]
      (let [pairs (partition 2 1 (:points data))]
        (dom/g nil
               (om/build-all segment pairs))))))


(defn main []
  (om/root path app-state
           {:target (. js/document (getElementById "container"))}))

(main)
