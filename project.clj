(defproject alumbra/analyzer "0.1.8"
  :description "GraphQL Schema/Operation Transformations"
  :url "https://github.com/alumbra/alumbra.analyzer"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"
            :year 2016
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
                 [alumbra/spec "0.1.6" :scope "provided"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [com.rpl/specter "0.13.2"]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.9.0"]
                             [alumbra/parser "0.1.5"]
                             [alumbra/generators "0.2.2"]]}
             :codox
             {:plugins [[lein-codox "0.10.0"]]
              :dependencies [[codox-theme-rdash "0.1.1"]]
              :codox {:project {:name "alumbra.analyzer"}
                      :metadata {:doc/format :markdown}
                      :themes [:rdash]
                      :source-uri "https://github.com/alumbra/alumbra.analyzer/blob/v{version}/{filepath}#L{line}"
                      :namespaces [alumbra.analyzer]}}}
  :aliases {"codox" ["with-profile" "+codox" "codox"]}
  :pedantic? :abort)
