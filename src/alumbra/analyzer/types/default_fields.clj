(ns alumbra.analyzer.types.default-fields)

(defn default-type-fields
  [type-name]
  {"__typename"
   {:field-name           "__typename"
    :containing-type-name type-name
    :inline-directives    {}
    :arguments            {}
    :type-name            "String"
    :non-null?            true
    :type-description     {:type-name "String"
                                    :non-null? true}}})
