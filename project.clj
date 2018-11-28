(defproject BMH-IG "PFSP"
  :description "Descrição: Algoritmos BMH e IG_BLSP"
  :url "https://github.com/ViniciusTxr/algoritmos-flowshop"
  :license {:name "Public License"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot core
  :repl-options {:timeout 2000000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4g" "-server"]
  :profiles {:uberjar {:aot :all}})
