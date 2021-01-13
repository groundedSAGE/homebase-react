(ns homebase.react
  (:require
   ["react" :as react]
   [clojure.string]
   [cljs.reader]
   [homebase.js :as hbjs]
   ;[datascript.core :as d]
   [datahike.api :as d]
   [clojure.core.async]
   [hitchhiker.tree.utils.cljs.async :as ha]
   #_[datascript.impl.entity :as de]
   [datahike.impl.entity :as de]))

(defn try-hook [hook-name f]
  (try (f)
    (catch js/Error e
      (throw
       (js/Error.
        (condp re-find (goog.object/get e "message")
          #"No protocol method IDeref.-deref defined for type undefined"
          "HomebaseProvider context unavailable. <HomebaseProvider> must be declared by a parent component before homebase-react hooks can be used."
          (str (goog.object/get e "message") "\n"
               (some->> (goog.object/get e "stack")
                        (re-find (re-pattern (str hook-name ".*\\n(.*)\\n?")))
                        (second)
                        (clojure.string/trim)))))))))

(defn changed? [entities cached-entities]
  (if (not= (count entities) (count cached-entities))
    true
    (reduce (fn [_ e]
              (let [e ^de/Entity (.-_entity e)]
                (when (let [cached-e (get cached-entities (get e "id"))]
                        (if (nil? cached-e)
                          (reduced true)
                          (reduce (fn [_ [ks v]]
                                    (when (not= v (get-in e ks))
                                      (reduced true)))
                                  nil cached-e)))
                  (reduced true))))
            nil entities)))

(defn cache->js [entity cached-entities]
  (clj->js
   (reduce
    (fn [acc [ks v]] (assoc-in acc ks v))
    {} (get @cached-entities (get entity "id")))))

(defn touch-entity-cache [entity cached-entities]
  (set! ^js/Object (.-_recentlyTouchedAttributes entity) #js {})
  (set! ^de/Entity (.-_entity entity)
        (vary-meta
         ^de/Entity (.-_entity entity) merge
         {:HBEntity/get-cb
          (fn [[e ks v]]
            (if (get e "id")
              (do
                (swap! cached-entities assoc-in [(get e "id") ks] v)
                (set! ^js/Object (.-_recentlyTouchedAttributes entity) 
                      (cache->js e cached-entities)))
              (do
                (reset! cached-entities {})
                (set! ^js/Object (.-_recentlyTouchedAttributes entity) #js {}))))}))
  entity)

(defn datom-select-keys [d]
  #js [(:e d) (str (:a d)) (:v d) (:tx d) (:added d)])

(defn datoms->js [datoms]
  (-> datoms
      into-array
      (.map datom-select-keys)))

;; (defn datoms->json [datoms]
;;   (reduce
;;    (fn [acc {:keys [e a v]}] 
;;      (assoc-in acc [e (namespace a) (name a)] v))
;;    {} datoms))

(defonce ^:export homebase-context (react/createContext))

(def base-schema
  {} #_{:db/ident {:db/unique :db.unique/identity}})

(def cfg-idb {:store  {:backend :indexeddb :id "idb-sandbox"}
              :keep-history? false
              :schema-flexibility :read})


(defn ^:export HomebaseProvider [props]
  (let [conn (atom nil)
        #_conn #_(d/create-conn (if-let [schema (goog.object/getValueByKeys props #js ["config" "schema"])]
                              (merge (hbjs/js->schema schema) base-schema)
                              base-schema))]
    (ha/go-try
     (ha/<? (d/create-database cfg-idb))
     (reset! conn (ha/<? (d/connect cfg-idb)))
     (when-let [tx (goog.object/getValueByKeys props #js ["config" "initialData"])]
       (ha/<? (hbjs/transact! @conn tx)))
     (let [conn @conn] ; tests that we actually create the database and can get the data. Must delete indexeddb store before refreshing counter page
       (println "get the result: " (ha/<? ((ha/<? (d/entity @conn :counter)) :counter/count)))))
    
    nil
    #_(react/createElement
     (goog.object/get homebase-context "Provider") 
     #js {:value @conn}  ;will need to deref the atom we put the database in here. Because that also needs to be deref'd. 
     (goog.object/get props "children"))))

#_(defn ^:export useClient []
  (let [conn (react/useContext homebase-context)
        key (react/useMemo rand #js [])
        client #js {"dbToString" (react/useCallback
                                  #(pr-str @conn)
                                  #js [])
                    "dbFromString" (react/useCallback
                                    #(do (reset! conn (cljs.reader/read-string %))
                                         (d/transact! conn [] ::silent))
                                    #js [])
                    "dbToDatoms" (react/useCallback
                                  #(datoms->js (d/datoms @conn :eavt))
                                  #js [])
                    ;; "dbToJSON" (react/useCallback
                    ;;             #(clj->js (datoms->json (d/datoms @conn :eavt)))
                    ;;             #js [])
                    "transactSilently" (react/useCallback
                                        (fn [tx] (try-hook "useClient" #(hbjs/transact! conn tx ::silent)))
                                        #js [])
                    #_"addTransactListener" #_(react/useCallback
                                           (fn [listener-fn] (d/listen! conn key #(when (not= ::silent (:tx-meta %))
                                                                                   (listener-fn (datoms->js (:tx-data %))))))
                                           #js [])
                    #_"removeTransactListener" #_(react/useCallback
                                              #(d/unlisten! conn key)
                                              #js [])}]
    [client]))
  
(defn ^:export useEntity [lookup]
  (println "the lookup: " lookup)
  (let [conn (react/useContext homebase-context)
        cached-entities (react/useMemo #(atom {}) #js [])
        run-lookup (react/useCallback
                    (fn run-lookup []
                      (touch-entity-cache
                       (try-hook "useEntity" #(hbjs/entity conn lookup))
                       cached-entities))
                    #js [lookup])
        [result setResult] (react/useState (run-lookup))
        listener (react/useCallback
                  (fn entity-listener []
                    (let [result (run-lookup)]
                      (when (changed? #js [result] @cached-entities)
                        (setResult result))))
                  #js [run-lookup])]
    #_(react/useEffect
     (fn use-entity-effect []
       (let [key (rand)]
         (d/listen! conn key listener)
         #(d/unlisten! conn key)))
     #js [lookup])
    [result]))

#_(defn ^:export useQuery [query & args]
  (let [conn (react/useContext homebase-context)
        cached-entities (react/useMemo #(atom {}) #js [])
        run-query (react/useCallback 
                   (fn run-query []
                     (.map (try-hook "useQuery" #(apply hbjs/q query conn args))
                           (fn [e] (touch-entity-cache e cached-entities))))
                   #js [query args])
        [result setResult] (react/useState (run-query))
        listener (react/useCallback
                  (fn query-listener []
                    (let [result (run-query)]
                      (when (changed? result @cached-entities)
                        (setResult result))))
                  #js [run-query])]
    #_(react/useEffect
     (fn use-query-effect []
       (let [key (rand)]
         (d/listen! conn key listener)
         #(d/unlisten! conn key)))
     #js [query args])
    [result]))

#_(defn ^:export useTransact []
  (let [conn (react/useContext homebase-context)
        transact (react/useCallback 
                  (fn transact [tx] (try-hook "useTransact" #(hbjs/transact! conn tx)))
                  #js [])]
    [transact]))