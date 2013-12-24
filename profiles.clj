{:user
 {:dependencies [[org.clojure/tools.namespace "0.2.4"]
                 [slamhound "1.5.0"]]
  :plugins [[lein-bin "0.3.4"]]
  :jvm-opts ["-Xmx3g"]
  :repl-options {:init
                 (do (use '[clojure.tools.namespace.repl :only (refresh)])
                     (set! *print-length* 103)
                     (set! *print-level* 15))}}}
