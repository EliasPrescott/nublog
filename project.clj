(defproject nublog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [hiccup "2.0.0"]
                 [clojure.java-time "1.4.3"]]
  :main ^:skip-aot nublog.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
