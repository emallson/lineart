(set-env!
 :source-paths #{"src"}
 :resource-paths #{"res"}
 :dependencies '[[adzerk/boot-cljs "0.0-3308-0" :scope "test"]
                 [adzerk/boot-cljs-repl "0.1.9" :scope "test"]
                 [adzerk/boot-reload "0.3.1" :scope "test"]
                 [pandeiro/boot-http "0.6.3-SNAPSHOT" :scope "test"]
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [org.omcljs/om "0.9.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]])

(require
 '[adzerk.boot-cljs :refer :all]
 '[adzerk.boot-cljs-repl :refer :all]
 '[adzerk.boot-reload :refer :all]
 '[pandeiro.boot-http :refer [serve]])

(deftask dev
  []
  (comp
   (serve :dir "target")
   (watch)
   (cljs :source-maps true
         :optimizations :none
         :unified-mode true)))
