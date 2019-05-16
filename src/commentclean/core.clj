(ns commentclean.core)

#_(defn clean-comment [text]
   (.replaceAll 
     text "//.*|(\"(?:\\\\[^\"]|\\\\\"|.)*?\")|(?s)/\\*.*?\\*/", "$1 "))

(defmulti comment-replace (fn [state ch the-rest] state))

(defn next-comment [ch the-rest]
   (if (= (first (comment-replace :slash (first the-rest) nil)) :idle)
     ch
     " "))

(defmethod comment-replace :idle [state ch the-rest]
  (condp = ch
   \/ [:slash (next-comment ch the-rest)] 
   \" [:string ch]
   [state ch])
  )

(defmethod comment-replace :slash [state ch _]
  (condp = ch
   \/ [:single-comment " "]
   \* [:multi-comment " "]
   [:idle ch])
  )

(defmethod comment-replace :single-comment [state ch _]
  (condp = ch
   \newline [:idle ch]
   [state " "])
  )

(defmethod comment-replace :multi-comment [state ch _]
  (condp = ch
   \* [:star " "]
   \newline [state ch]
   [state " "])
  )

(defmethod comment-replace :star [state ch _]
  (condp = ch
   \/ [:idle " "]
   \newline [state ch]
   [state " "])
  )

(defmethod comment-replace :string [state ch _]
  (condp = ch
   \\ [:backslash ch]
   \" [:idle ch]
   [state ch])
  )

(defmethod comment-replace :backslash [state ch _]
  [:string ch])

(defn clean [text]
  (loop [t text
         state :idle
         res []]
    (if (not (empty? t))
      (let [the-rest (rest t) 
            [state ch] (comment-replace state (first t) the-rest)]
        (recur the-rest state (conj res ch)) )
      (apply str res))))

