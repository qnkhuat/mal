(ns mal.env)

(defn env-create 
  ([] (atom { :outer nil :env {} }))
  ([outer] (atom { :outer outer :env {} }))
  ([outer binds exprs] 
   (let [env (env-create outer)]
     (doall (map (fn [k v] (env k v)) binds exprs))
     env))
  )

(defn inc-pos [a] (swap! a update-in [:pos] inc))

(defn env-set [env k v]
  (swap! env (fn [env] (assoc-in env  [:env k] v)))
  v)

(defn env-find [env k]
  (if (contains? (:env @env) k)
    env
    (if (nil? (:outer @env))
      nil
      (env-find (:outer @env) k))
    ))


(defn env-get [env k]
  (let [contained-env (env-find env k)]
    (if (nil? contained-env)
      (throw (Exception. (format "%s not found" k)))
      (get (:env @contained-env) k))
    ))

