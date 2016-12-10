(defproject emulator-apple2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.211" :scope "provided"]
                 [reagent "0.5.1"]
                 [reagent-forms "0.5.22"]
                 [reagent-utils "0.1.7"]
                 [secretary "1.2.3"]]

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-figwheel "0.5.4-7"]]

  :min-lein-version "2.5.0"

  :clean-targets ^{:protect false} [:target-path "public/js/app.js" "public/js/out"]

  :resource-paths ["public"]

  ;:figwheel-options {:http-server-root "public"
  ;                   :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]
  ;                   :css-dirs         ["public/css"]}

  :profiles {:dev  {:dependencies [[figwheel-sidecar "0.5.4-7"]
                                   [com.cemerick/piggieback "0.2.1"]
                                   [proto-repl "0.3.1"]]

                    :source-paths ["src" "env/dev/clj"]

                    :cljsbuild    {:builds [{:id           "app"
                                             :source-paths ["src" "env/dev/cljs"]
                                             :compiler     {:main          "emulator-apple2.dev"
                                                            :output-to     "public/js/app.js"
                                                            :output-dir    "public/js/out"
                                                            :asset-path    "js/out"
                                                            :optimizations :none
                                                            :pretty-print  true
                                                            :source-map    true}}]}

                    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}

             :prod {:cljsbuild {:jar    true
                                :builds [{:id           "app"
                                          :source-paths ["src" "env/prod/cljs"]
                                          :compiler
                                                        {:output-to     "public/js/app.js"
                                                         :output-dir    "public/js/out"
                                                         :asset-path    "js/out"
                                                         :optimizations :advanced
                                                         :pretty-print  false}}]}}})
