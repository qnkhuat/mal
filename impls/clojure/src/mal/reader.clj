(ns mal.reader
  (:gen-class))

(declare read_form read_list read_atom read_str tokenize)

(defn throw-str [s]
  (throw (Exception. s)))

(defn inc-pos [a] (swap! a update-in [:pos] inc))

(defn curr-token [a-tokens] (nth (:tokens @a-tokens) (:pos @a-tokens) nil))

(defn next-token [a-tokens] (nth (:tokens @a-tokens) (inc (:pos @a-tokens)) nil))

; Call tokenize and create a new reader object
(defn read_str [s] 
  (read_form (tokenize s)))

; Take a string strung return an array/list of all tokens
(def tok-re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")
(defn tokenize [s] 
  (atom {:tokens (map second (re-seq tok-re s))
         :pos 0}))

(defn read_form [a-tokens] 
  (let [tok (curr-token a-tokens)
        pos (:pos a-tokens)]
    (if (= tok "(")
          (read_list a-tokens)
          (read_atom tok))))

(defn read_list [a-tokens] 
  (loop [result []]
    (do 
      (if (< (:pos @a-tokens) (count (:tokens @a-tokens)))
        (inc-pos a-tokens)
        (throw-str "Expected ), got EOF"))
      (if (= (curr-token a-tokens) ")")
        result
        (recur (conj result (read_form a-tokens) ))))))

(defn read_atom [tok] 
  (cond (= tok "true") true
        (= tok "false") false
        (= tok "nil") nil
        (= tok nil) nil
        :else (read-string tok)
        ))


