(defproject com.workiva/recide "1.0.0"
  :description "Provides utilities for defining standard ex-info forms, as well as the capacity for checking at compile-time that they are being used as intended"
  :url "https://github.com/Workiva/recide"
  :license {:name "Apache License, Version 2.0"}
  :plugins [[lein-cljfmt "0.6.4"]
            [lein-codox "0.10.3"]
            [lein-shell "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.clojure/data.fressian "0.2.1"]
                 [com.workiva/utiliva "0.1.0"]]

  :deploy-repositories {"clojars"
                        {:url "https://repo.clojars.org"
                         :sign-releases false}}

  :source-paths ["src"]
  :test-paths ["test"]
  :java-source-paths ["java-src"]

  :global-vars {*warn-on-reflection* true}

  :aliases {"docs" ["do" "clean-docs," "with-profile" "docs" "codox," "java-docs"]
            "clean-docs" ["shell" "rm" "-rf" "./documentation"]
            "java-docs" ["shell" "javadoc" "-d" "./documentation/java"
                         "./java-src/recide/error/ErrorForm.java"
                         "./java-src/recide/error/IErrorForm.java"
                         "./java-src/recide/sanex/ISanitizable.java"
                         "./java-src/recide/sanex/ISanitized.java"
                         "./java-src/recide/sanex/Utils.java"]}

  :codox {:metadata {:doc/format :markdown}
          :themes [:rdash]
          :output-path "documentation/clojure"}

  :profiles {:dev [{:dependencies [[criterium "0.4.3"]]
                    :jvm-opts ["-Drecide.capture-insists=true"]}]
             :docs {:dependencies [[codox-theme-rdash "0.1.2"]]}
             :aot {:aot [recide.core]}})
