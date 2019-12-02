(ns clojure-exercises.core
  (:gen-class))

(use 'clojure.tools.trace)

(defmacro infix
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

(defn read-sentence
  [path]
  (-> path 
     clojure.java.io/resource
     slurp
     read-string))

(defmacro infix-2
  [[operand1 operator operand2]]
  (list operator operand1 operand2))

(defn test-1 
  [abc def]
  (+ abc def))

(defn something-1
  [a b c d]
  (map #(apply test-1 %) [[a b] 
                          [c d]]))

(defmacro myand
  ([] true)
  ([x] x)
  ([x & next] 
   `(let [and# ~x] 
       (if and# 
         (myand ~@next) 
         and#))))

(defmacro mywhen
  [test & body]
  (list 'if test (cons 'do body)))

(defn code-criticize
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic
  [bad good]
  `(do 
      ~(code-criticize "Bad code:" bad)
      ~(code-criticize "Good code:" good)))

(defmacro code-critic-2
  [bad good]
  `(do ~@(map #(apply code-criticize %)  [["Bad code:" bad]
                                          ["Good code:" good]])))

(future (Thread/sleep 10)
  (println "ill print after 0.01 secs"))
(println "ill print immediately")


(let [result (future (println "prints once")
               (+ 1 1))]
  (println "defref:" (deref result))
  (println "@:" @result))

(let [f (future)]
  @f
  (println "@'ed:" (realized? f)))


(let [f (future)]
  (println "not @:" (realized? f)))


(time (def jackson-5 
        (delay (let [message "just call my name and ill be there"]
                 (println "message:" message)
                 message))))

(defmacro wait 
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~(time (println "Hello im russell"))
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

(time @(-> (enqueue saying (wait 200 "Hello govna") (println @saying))
           (enqueue saying (wait 400 "Pip Pip") (println @saying))
           (enqueue saying (wait 100 "Cheerios") (println @saying))))
