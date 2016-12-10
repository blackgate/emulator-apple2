(ns emulator-apple2.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [emulator-apple2.cpu :as c]
            [emulator-apple2.memory :as m]
            [emulator-apple2.utils :as u]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(defonce ui-state (atom {:blink false}))

(defn initial-canvas-ctx [canvas width height]
  (let [ctx (.getContext canvas "2d")]
    (set! (.-fillStyle ctx) "black")
    (.fillRect ctx 0 0 width height)
    ctx))

(defn update-screen! []
  (c/reset-cpu!)
  (let [canvas (.getElementById js/document "screen")
        width (.-width canvas)
        height (.-height canvas)
        ctx (initial-canvas-ctx canvas width height)
        img-data (.getImageData ctx 0 0 width height)]
    (go-loop [start (<! u/frames-stream)
              end (<! u/frames-stream)]
      (let [span (/ 1000.0 (- end start))]
        (c/cpu-exec-cycles! (/ 1000000 (if (< span 1) 1 span)))
        (m/draw-screen! (.-data img-data) (:blink @ui-state))
        (.putImageData ctx img-data 0 0)
        (recur end (<! u/frames-stream))))))

(defn blink-cursor []
  (go-loop []
    (<! (timeout 267))
    (swap! ui-state update :blink not)
    (recur)))

(defn key-modifier [key-ev]
  (let [key (.-which key-ev) ctrl? (.-ctrlKey key-ev)]
    (if (and ctrl? (> key 0x40))
      (bit-flip key 6)
      key)))

(defn keyboard-input! []
  (go-loop []
    (let [key-ev (<! u/keys-stream)
          key (key-modifier key-ev)]
      (when-not (nil? key)
        (m/send-key! key)))
    (recur)))

(defn init! []
  (set! c/*mem-read-fn* m/read-mem)
  (set! c/*mem-write-fn* m/write-mem!)
  (update-screen!)
  (blink-cursor)
  (keyboard-input!))
