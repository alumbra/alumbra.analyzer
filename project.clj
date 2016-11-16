(defproject alumbra/analyzer "0.1.0-SNAPSHOT"
  :description "GraphQL Schema/Operation Transformations"
  :url "https://github.com/alumbra/alumbra.analyzer"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"
            :year 2016
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [com.rpl/specter "0.13.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [alumbra/spec "0.1.0-SNAPSHOT"]
                                  [alumbra/parser "0.1.0-SNAPSHOT"]
                                  [alumbra/generators "0.1.1"]]}}
  :pedantic? :abort)
