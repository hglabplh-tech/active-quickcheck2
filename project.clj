(defproject de.active-group/active-quickcheck2 "0.1.0-SNAPSHOT"
  :description "QuickCheck clone for Clojure"
  :url "http://github.com/active-group/active-quickcheck2"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 ;; FIXME: only for active.monad, hopefully
                 [de.active-group/active-clojure "0.43.0-SNAPSHOT"]
                 ;; FIXME: this is probably only expt, remove
                 [org.clojure/math.numeric-tower "0.1.0"]
                 [de.active-group/active-data "0.2.1"]]

  :global-vars {*warn-on-reflection* true})
