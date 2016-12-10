(ns emulator-apple2.memory
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [emulator-apple2.rom :as rom]
            [emulator-apple2.utils :as u]
            [emulator-apple2.charset :as c]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(def GFX-TEXT-FLAG 2r0001)
(def GFX-MIXED-FLAG 2r0010)
(def GFX-PAGE2-FLAG 2r0100)
(def GFX-HIRES-FLAG 2r1000)
(def SCREEN-WIDTH 280)
(def LOWRES-PATTERN #js[0x00 0x00 0x00 0x00 0xFF 0xFF 0xFF 0xFF])
(def LOWRES-COLORS
  (clj->js
    [[0x00, 0x00, 0x00],                                    ; black
     [0xdd, 0x00, 0x33],                                    ; 0x1 deep red
     [0x00, 0x00, 0x99],                                    ; 0x2 dark blue
     [0xdd, 0x00, 0xdd],                                    ; 0x3 purple
     [0x00, 0x77, 0x00],                                    ; 0x4 dark green
     [0x55, 0x55, 0x55],                                    ; 0x5 dark gray
     [0x23, 0x22, 0xff],                                    ; 0x6 medium blue
     [0x66, 0xaa, 0xff],                                    ; 0x7 light blue
     [0x88, 0x55, 0x22],                                    ; 0x8 brown
     [0xff, 0x66, 0x00],                                    ; 0x9 orange
     [0xaa, 0xaa, 0xaa],                                    ; 0xa light gray
     [0xff, 0x99, 0x88],                                    ; 0xb pink
     [0x00, 0xdd, 0x00],                                    ; 0xc green
     [0xff, 0xff, 0x00],                                    ; 0xd yellow
     [0x00, 0xff, 0x99],                                    ; 0xe aquamarine
     [0xff, 0xff, 0xff]]))                                  ; 0xf white

(defonce *mem* (u/make-filled-array 0 0x10000))
(defonce *graphics-mode* 0)

(defn set-graphics-flags! [flags]
  (set! *graphics-mode* (bit-or *graphics-mode* flags)))

(defn clear-graphics-flags! [flags]
  (set! *graphics-mode* (bit-and-not *graphics-mode* flags)))

(defn graphics-flags-set? [flags]
  (== flags (bit-and *graphics-mode* flags)))

(defn io-callbacks! [addr write val]
  (case addr
    0xC010 (aset *mem* 0xC000 (bit-and (aget *mem* 0xC000) 0x7F))
    0xC050 (clear-graphics-flags! GFX-TEXT-FLAG)
    0xC051 (set-graphics-flags! GFX-TEXT-FLAG)
    0xC052 (clear-graphics-flags! GFX-MIXED-FLAG)
    0xC053 (set-graphics-flags! GFX-MIXED-FLAG)
    0xC054 (clear-graphics-flags! GFX-PAGE2-FLAG)
    0xC055 (set-graphics-flags! GFX-PAGE2-FLAG)
    0xC056 (clear-graphics-flags! GFX-HIRES-FLAG)
    0xC057 (set-graphics-flags! GFX-HIRES-FLAG)
    nil))

(defn write-mem! [addr val]
  (io-callbacks! addr true val)
  (when (< addr 0xC000)
    (aset *mem* addr val)))

(defn read-mem [addr]
  (if-let [val (io-callbacks! addr false nil)]
    val
    (if (> addr 0xCFFF)
      (aget rom/data (- addr 0xD000))
      (aget *mem* addr))))


;; Keyboard

(defn send-key! [key]
  (aset *mem* 0xC000 (bit-or key 0x80)))

(defn send-kbd-input! [input]
  (go-loop
    [[ch & r] input]
    (send-key! (.charCodeAt ch))
    (<! (timeout 50))
    (when-not (empty? r)
      (recur r))))


;; Screen - Text Mode

(defn get-txt-row-addr [row]
  (let [byte1 (bit-or (bit-and (bit-shift-right row 1) 0x03) 0x04)
        a (+ (bit-and row 0x18) (if (> (bit-and row 1) 0) 0x80 0))
        byte2 (bit-or (bit-and (bit-shift-left a 2) 0xFF) a)]
    (u/get-word byte2 byte1)))

(defn draw-pixel! [data x y color]
  (let [p (+ (* y SCREEN-WIDTH 4) (* x 4))]
    (aset data p (aget color 0))
    (aset data (+ p 1) (aget color 1))
    (aset data (+ p 2) (aget color 2))))

(defn get-txt-row [row]
  (let [sec-page? (graphics-flags-set? GFX-PAGE2-FLAG)
        row-addr (+ (get-txt-row-addr row) (if sec-page? 0x400 0))]
    (.slice *mem* row-addr (+ row-addr 40))))

(defn draw-char-row! [data x y row bg-color fg-color]
  (loop [i 6 r row]
    (let [color (if (bit-test r 0) fg-color bg-color)]
      (draw-pixel! data (+ x i) y color)
      (when (> i 0)
        (recur (dec i) (bit-shift-right r 1))))))

(defn draw-char! [data row col ch bg-color fg-color]
  (let [x (* col 7)
        y (* row 8)]
    (dotimes [i 8]
      (draw-char-row! data x (+ y i) (aget ch i) bg-color fg-color))))

(defn draw-txt-pixels! [data mixed blink]
  (dotimes [row (if mixed 4 24)]
    (let [r (+ row (if mixed 20 0))
          ch-row (get-txt-row r)]
      (dotimes [c 40]
        (let [ch (aget ch-row c)
              inv-ch (or (bit-test ch 7) (and blink (bit-test ch 6)))
              ch-pattern (aget c/data (bit-and ch 2r111111))
              bg-color (if inv-ch #js[0 0 0] #js[0 0xFF 0])
              fg-color (if inv-ch #js[0 0xFF 0] #js[0 0 0])]
          (draw-char! data r c ch-pattern bg-color fg-color))))))

(defn draw-lowres-pixels! [data mixed]
  (dotimes [r (if mixed 20 24)]
    (let [ch-row (get-txt-row r)]
      (dotimes [c 40]
        (let [ch (aget ch-row c)
              bg-color (aget LOWRES-COLORS (bit-and ch 0xF))
              fg-color (aget LOWRES-COLORS (bit-shift-right ch 4))]
          (draw-char! data r c LOWRES-PATTERN bg-color fg-color))))))

(defn draw-hires-sprite! [data r c sr fg-color bg-color]
  (let [y (+ (* r 8) sr)
        x (* c 7)
        mem-addr (+ (get-txt-row-addr r) (* sr 0x400) 0x1C00 c)]
    (loop [i 0 m (read-mem mem-addr)]
      (let [color (if (bit-test m 0) fg-color bg-color)]
        (draw-pixel! data (+ x i) y color)
        (when (< i 6)
          (recur (inc i) (bit-shift-right m 1)))))))

(defn draw-hires-pixels! [data mixed]
  (dotimes [r (if mixed 20 24)]
    (dotimes [sr 8]
      (dotimes [c 40]
        (let [bg-color #js[0 0 0]
              fg-color #js[0x00 0xFF 0x00]]
          (draw-hires-sprite! data r c sr fg-color bg-color))))))

(defn draw-screen! [data blink]
  (if (graphics-flags-set? GFX-TEXT-FLAG)
    (draw-txt-pixels! data false blink)
    (let [mixed (graphics-flags-set? GFX-MIXED-FLAG)]
      (when mixed
        (draw-txt-pixels! data mixed blink))
      (if (graphics-flags-set? GFX-HIRES-FLAG)
        (draw-hires-pixels! data mixed)
        (draw-lowres-pixels! data mixed)))))
