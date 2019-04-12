(ns resilience4clj.core)

(defn ^:private anom-map
  [category msg]
  {:resilience4clj.anomaly/category (keyword "resilience4clj.anomaly"
                                             (name category))
   :resilience4clj.anomaly/message msg})

(defn anomaly!
  ([name msg]
   (throw (ex-info msg (anom-map name msg))))
  ([name msg cause]
   (throw (ex-info msg (anom-map name msg) cause))))

(defprotocol IDecorator
  (success [this])
  (failure [this]))

(defn create-effect
  [f]
  (reify IDecorator
    (success [_]
      (fn [ret & args]
        (future (apply f (conj args ret)))
        ret))
    (failure [_]
      nil)))

(defn create-fallback
  [f]
  (reify IDecorator
    (success [_]
      nil)
    (failure [_]
      f)))

(defn decorate
  [f decorator]
  (fn [& args]
    (try
      (let [ret (apply f args)]
        (if-let [success (success decorator)]
          (apply success (conj args ret))
          ret))
      (catch Throwable e
        (if-let [failure (failure decorator)]
          (apply failure (conj args e))
          (throw e))))))


(comment
  (def effect (create-effect (fn [ret n]
                               (Thread/sleep 1000)
                               (println "effect from " n))))

  (def fallback (create-fallback (fn [e n opts]
                                   (println "failed for" n)
                                   "dummy return")))

  (defn hello
    ([n]
     (hello n nil))
    ([n {:keys [fail?]}]
     (if fail?
       (throw (ex-info "Hello failed!" {}))
       (str "Hello, " n))))

  (def power-hello
    (decorate hello effect))


  (def power-hello2
    (-> hello
        (decorate effect)
        (decorate fallback))))
