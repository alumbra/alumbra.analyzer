nil (do (set! *warn-on-reflection* nil) (clojure.core/require (quote codox.main)) (codox.main/generate-docs (quote {:description "GraphQL Schema/Operation Transformations", :package alumbra/analyzer, :source-uri "https://github.com/alumbra/alumbra.analyzer/blob/v{version}/{filepath}#L{line}", :namespaces [alumbra.analyzer], :output-path "/git/github/alumbra-project/alumbra.analyzer/target/doc", :name "alumbra.analyzer", :source-paths ("/git/github/alumbra-project/alumbra.analyzer/src"), :themes [:rdash], :project {:name "alumbra.analyzer"}, :root-path "/git/github/alumbra-project/alumbra.analyzer", :version "0.1.2", :metadata {:doc/format :markdown}})))