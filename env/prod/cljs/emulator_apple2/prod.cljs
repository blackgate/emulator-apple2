(ns emulator-apple2.prod
  (:require [emulator-apple2.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
