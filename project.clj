(defproject com.exoscale/lingo "1.0.0-alpha1-SNAPSHOT"
  :description ""
  :url "https://github.com/exoscale/lingo"
  :dependencies [[org.clojure/clojure "1.10.2"]]
  :global-vars {*warn-on-reflection* true}

  :repositories [["exoscale" {:url "https://artifacts.exoscale.ch"}]]
  :deploy-repositories [["releases" {:url "s3p://exo-artifacts/releases"
                                     :no-auth true
                                     :sign-releases false}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
