(ns mal.step0-repl
  (:gen-class))

(defn READ [s] s)

(defn EVAL [s] s)

(defn PRINT [s] (println s))

; Pipeline
(defn rep [str] (PRINT (EVAL (READ str))))

(defn -main [] 
  (do
    (print "user> ")
    (flush)
    (let [line (read-line)]
      (if (nil? line)
        (System/exit 0)
        (do
          (rep line)
          (-main))
        ))))
