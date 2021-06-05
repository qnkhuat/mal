(ns mal.util)

(defmacro condv
  [& clauses]
  (when clauses
    (list
      'if
      (first clauses)
      (if (next clauses)
        `(do (println (str "condv " '~(first clauses)))
             ~(second clauses))
        (throw (IllegalArgumentException.
                 "cond requires an even number of forms")))
      (cons 'condv (next (next clauses))))))

