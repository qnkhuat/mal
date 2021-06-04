(ns mal.reader)

(declare read-form read-list read-atom read-str tokenize)

(defn curr-token [a-tokens] (nth (:tokens @a-tokens) (:pos @a-tokens) nil))

(defn next-token [a-tokens] (nth (:tokens @a-tokens) (inc (:pos @a-tokens)) nil))

(defn inc-pos [a] (swap! a update-in [:pos] inc))

(defn safe-inc-pos [a msg]
  (if (next-token a)
    (inc-pos a)
    (throw (Exception. msg))))

; Call tokenize and create a new reader object
(defn read-str [s] 
  (read-form (tokenize s)))

; Take a string strung return an array/list of all tokens
(def tok-re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`,;]*)")
(defn tokenize [s] 
  (atom {:tokens (filter #(not= "" %) (map second (re-seq tok-re s)))
         :pos 0}))

(defn read-list [a-tokens] 
  (loop [result '()]
    (do 
      (safe-inc-pos a-tokens "Expected ), got EOF")
      (if (= (curr-token a-tokens) ")")
        (reverse result)
        (recur (conj result (read-form a-tokens) ))))))

(defn read-vector [a-tokens] 
  (loop [result []]
    (do 
      (safe-inc-pos a-tokens "Expected ], got EOF")
      (if (= (curr-token a-tokens) "]")
        result
        (recur (conj result (read-form a-tokens) ))))))


(defn read-map [a-tokens]
  (loop [result {}]
    (do 
      (safe-inc-pos a-tokens "Expected }, got EOF")
      (if (= (curr-token a-tokens) "}")
        result
        (let [k (read-form a-tokens)
              _ (safe-inc-pos a-tokens "Expected }, got EOF") ; eat the value
              v (read-form a-tokens)]
          (recur (assoc result k v)))))))


(def quote-re #"['`~@]|~@")
(defn read-form [a-tokens] 
  (let [tok (curr-token a-tokens)
        pos (:pos a-tokens)]
    (cond (= tok "(") (read-list a-tokens)
          (= tok "[") (read-vector a-tokens)
          (= tok "{") (read-map a-tokens)
          (re-matches quote-re tok) (do
                                      (safe-inc-pos a-tokens "got EOF")
                                      (list (read-atom tok) (read-form a-tokens)))
          (= tok "^") (do
                        (safe-inc-pos a-tokens "got EOF")
                        (let [m (read-form a-tokens)]
                          (safe-inc-pos a-tokens "got EOF")
                          (list (read-atom tok) (read-form a-tokens) m))
                        )
          :else (read-atom tok)
          )))

(defn unescape [s]
  (-> s (clojure.string/replace "\\\\" "\u029e")
      (clojure.string/replace "\\\"" "\"")
      (clojure.string/replace "\\n" "\n")
      (clojure.string/replace "\u029e" "\\")))

(def badstr-re #"^\"")
(def int-re #"^-?[0-9]+$")
(def str-re #"^\"((?:[\\].|[^\\\"])*)\"$")
(defn read-atom [tok] 
  (cond (= tok "true") true
        (= tok "false") false
        (= tok "nil") nil
        (= tok "'") 'quote
        (= tok "`") 'quasiquote
        (= tok "~") 'unquote
        (= tok "@") 'deref
        (= tok "^") 'with-meta
        (= tok "~@") 'splice-unquote
        (clojure.string/starts-with? tok ":") (symbol tok) ; keyword 
        (re-seq int-re tok) (read-string tok)
        (re-seq str-re tok) (unescape (second (re-find str-re tok)))
        (re-seq badstr-re tok) (throw (Exception. (str "expected '\"', got EOF")))
        :else (symbol tok)
        ))

