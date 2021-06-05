(ns mal.env)


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
      (throw (Exception. (format "%s not found in env" k)))
      (get (:env @contained-env) k))
    ))


(defn env-create 
  ([] (atom { :outer nil :env {} }))
  ([outer] (atom { :outer outer :env {} }))
  ([outer binds exprs] 
   (let [env (env-create outer)]
     (do 
       (loop [b binds
              e exprs]
         (cond 
           (= nil b) env
           (= '& (first b)) (env-set env (nth b 1) e)
           :else (do
                   (env-set env (first b) (first e))
                   (recur (next b) (rest e))
                   )))
       env))))


;(def e (env-create nil ['& 'more] [ 3 4 5 ]))


