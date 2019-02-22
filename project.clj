(defproject cljtinyc "0.1.0-SNAPSHOT"
  :description "A C compiler written for my CS230 compilers class"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [instaparse "1.4.9"]
                 [slingshot "0.12.2"]
                 [org.clojure/tools.cli "0.4.1"]
                 [dk.brics.automaton/automaton "1.11.2"]]
  :main ^:skip-aot cljtinyc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
