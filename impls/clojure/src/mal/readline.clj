(ns mal.readlnie
  (:require [net.n01se.clojure-jna :as jna]))

; Unable to call
(def readline (jna/to-fn String readline/readline))
