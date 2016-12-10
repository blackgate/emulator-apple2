(ns emulator-apple2.utils
  (:require [cljs.core.async :refer [put! take! chan]]))

(defn get-word [byte1 byte2]
  (bit-or (bit-shift-left byte2 8) byte1))

(defn str-lpad [s pad num]
  (let [str-val (str s)
        rep (- num (count str-val))]
    (str (apply str (repeat rep pad)) str-val)))

(defn- request-anim-frames [cb-fn]
  (js/requestAnimationFrame
    (fn [ts]
      (cb-fn ts)
      (request-anim-frames cb-fn))))

(defonce
  frames-stream
  (let [frames (chan)]
    (request-anim-frames #(put! frames %))
    frames))

(def KEY-BS 8)
(def KEY-TAB 9)
(def KEY-CTRL 17)
(def KEY-ESC 27)

(defn- valid-key-down? [ev]
  (let [key (.-which ev)]
    (and
      (not= key KEY-CTRL) ; ctrl key alone
      (or (.-ctrlKey ev)
          (== key KEY-BS)
          (== key KEY-TAB)
          (== key KEY-ESC)))))

(defonce
  keys-stream
  (let [keys (chan)]
    (.addEventListener js/document "keydown" #(when (valid-key-down? %) (put! keys %)))
    (.addEventListener js/document "keypress" #(when-not (valid-key-down? %) (put! keys %)))
    keys))

(defn make-filled-array [value length]
  (loop [n 0 *arr* (make-array length)]
    (if (< n length)
      (do
        (aset *arr* n value)
        (recur (inc n) *arr*))
      *arr*)))