(ns user
    (:use [figwheel-sidecar.repl-api :as ra]))
;; This namespace is loaded automatically by nREPL

;; read project.clj to get build configs
(def profiles (->> "project.clj"
                   slurp
                   read-string
                   (drop-while #(not= % :profiles))
                   (apply hash-map)
                   :profiles))

(def cljs-builds (get-in profiles [:dev :cljsbuild :builds]))

(defn start-figwheel
      "Start figwheel for one or more builds"
      []
      (ra/start-figwheel!
        {:figwheel-options {:css-dirs ["public/css"]}
         :build-ids  ["app"]
         :all-builds cljs-builds})
      (ra/cljs-repl))

(defn stop-figwheel
      "Stops figwheel"
      []
      (ra/stop-figwheel!))
