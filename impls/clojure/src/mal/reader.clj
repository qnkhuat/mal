(ns mal.reader
  (:gen-class))

(declare read-form read-list read-atom read-str tokenize)

(defn throw-str [s]
  (throw (Exception. s)))

(defn curr-token [a-tokens] (nth (:tokens @a-tokens) (:pos @a-tokens) nil))

(defn next-token [a-tokens] (nth (:tokens @a-tokens) (inc (:pos @a-tokens)) nil))

(defn inc-pos [a] (swap! a update-in [:pos] inc))

(defn safe-inc-pos [a msg]
  (if (next-token a)
    (inc-pos a)
    (throw-str msg)))

; Call tokenize and create a new reader object
(defn read-str [s] 
  (read-form (tokenize s)))

; Take a string strung return an array/list of all tokens
(def tok-re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")
(defn tokenize [s] 
  (atom {:tokens (map second (re-seq tok-re s))
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


(def quote-re #"['`~@]")
(defn read-form [a-tokens] 
  (let [tok (curr-token a-tokens)
        pos (:pos a-tokens)
        ]
    (cond (= tok "(") (read-list a-tokens)
          (= tok "[") (read-vector a-tokens)
          (re-matches quote-re tok) (do
                                      (safe-inc-pos a-tokens "got EOF")
                                      (list (read-atom tok) (read-form a-tokens)))
          :else (read-atom tok)
          )))

(def symbol-re #"[-+/*]")
(defn read-atom [tok] 
  (cond (= tok "true") true
        (= tok "false") false
        (= tok "nil") nil
        (= tok "'") 'quote
        (= tok "`") 'quasiquote
        (= tok "~") 'unquote
        (= tok nil) nil
        (number? tok) tok
        (re-matches symbol-re tok) (symbol tok)
        :else (read-string tok)
        ))

