(ns perfbox.core
  (:gen-class)
  (:require
   [zen.core :as zen]
   [org.httpkit.client :as http]
   [cheshire.core]
   [sci.core]
   [clojure.string :as str]
   [clojure.walk]
   [clojure.edn]
   [prometheus.core]
   [taoensso.tufte :as tufte])
  (:import [java.util UUID]
           [java.net URLEncoder]))





(comment
  (def acc (tufte/add-accumulating-handler! "*"))
(tufte/add-basic-println-handler! {})

(def stat-accum (tufte/add-accumulating-handler! "*"))
  @acc

  @(second (tufte/profiled {:id :test}
                            (tufte/p :ss (Thread/sleep 500))
                            (tufte/p :ss (Thread/sleep 500))
                            (tufte/p [2 :ss] (Thread/sleep 500))
                            )
           )

  (tufte/profile {:id :test}
                  (tufte/p :ss (Thread/sleep 500))
                  (tufte/p :ss (Thread/sleep 500))
                  (tufte/p [2 :ss] (Thread/sleep 500))
                  )

  )

(declare ^:dynamic i)

(defn start-time []
  (System/currentTimeMillis))

(defn measure-time [start]
  (/ (- (System/currentTimeMillis) start)
     1000.0))


(defn url-encode [x]
  (URLEncoder/encode (str x) "UTF-8"))

(defn query-string [params]
  (->> params
       (reduce
         (fn [acc [k v]]
           (if (sequential? v)
             (into acc (->> v (mapv #(str (name k)"="(url-encode %)))))
             (conj acc (str (name k)"="(url-encode v)))
             ))
         [])
       (str/join "&")))

(defn request [ztx subj req lbls]
  (let [metrics (:metrics @ztx)
        start (start-time)
        url (str (:url subj) (:prefix subj) (:uri req))
        action (or (:action req)
                   (str (name (or (:method req) :get)) (:uri req)))]
    (prometheus.core/counter metrics :perf_request_count (assoc lbls :acount (:action req)))
    (when (:debug req)
      (println "\nStart: " (str "GET " (:uri req) "?" (query-string (merge (:common-params req) (:params req)))
                    "&_explain=on")
               "\n"))
    (http/request
     (merge req
            (cond-> {:url url
                     :method (or (:method req) :get)
                     :query-params (merge (:common-params req) (:params req))
                     :headers {"content-type" "application/json"}}
              (:basic-auth subj)
              (assoc :basic-auth (:basic-auth subj))
              (:data req)
              (assoc :body (cheshire.core/generate-string (:data req)))))
     (fn [{st :status ex :error :as res}]
       (let [d (measure-time start)]
         (prometheus.core/histogram
          metrics :perf_request_duration
          (assoc lbls :acount (:action req) :status (or st 500)) d))
       (when-let [slot (:slot-to-store req)]
         (let [result (-> (:body res)
                          (cheshire.core/parse-string true)
                          first
                          :result)]
           (assert (seq result) "No data was fetched")
           (swap! ztx assoc-in [:persist-store slot] result)))
       (when (or (:debug req)
                 ex
                 (= (:status res) 500))
         (println "\nStatus: " (:status res) " in " (:x-duration (:headers res)) "ms." " --> \n"
                  (str "GET " (:uri req) "?" (query-string (merge (:common-params req) (:params req)))
                       "&_explain=on"
                       )
                  "\n"
                  (or st (.getMessage ex))
                  "\n"
                  )
         #_(println (:method req) url (merge (:common-params req) (:params req)) (:action req) (or st (.getMessage ex)) )
         #_(clojure.pprint/pprint res)
         #_(println " body: " (:body res)))))))

(defn run-action [ztx {subj  :subj
                       {ops :ops :as suite} :suite
                       thr-idx :thr-idx
                       act :act
                       i :step
                       lbls :labels
                       :as opts}]
  (let [idx (* (or i 1) (inc (or thr-idx 0)))
        common-params (get-in opts [:suite :common-params])
        debug (get-in opts [:suite :debug])
        act (sci.core/eval-form (sci.core/init {:bindings {'i idx 't thr-idx 'o i
                                                           'persist-store (:persist-store @ztx)}}) act)]
    (when (= 0 (mod idx 1000))
      (print ".")
      (flush))
    (tufte/p :combined
             (tufte/p (:action act)
                      @(request ztx subj
                                (cond-> (assoc act :common-params common-params)
                                  debug (assoc :debug true))
                                lbls)))))


(defn test-init [ztx {test :test thr-idx :thr-idx :as opts}]
  (doseq [act (:init test)]
    (run-action ztx (assoc opts :act act)))
  opts)

(defn fix-symbols [x]
  (clojure.walk/postwalk
   (fn [x] (if (symbol? x)
            (symbol (name x))
            x)) x))

(defn run-test [ztx {subj :subj
                     suite :suite
                     tstart :start
                     longest-test :longest-test
                     test-nm :test
                     i :step
                     thr-idx :thr-idx
                     duration-sec :duration-sec
                     ops :ops
                     :as opts}]
  (if-let [test (-> (zen/get-symbol ztx test-nm)
                    (fix-symbols))]
    (doseq [[idx act]   (map-indexed vector
                               (if longest-test
                                 (take longest-test (cycle (:steps test)))
                                 (:steps test)))]
      (run-action ztx (assoc opts :act act :step (* i (inc idx)))))
    (throw (Exception. (str "No test " test-nm)))))

(defn run-test-inits [ztx {subj :subj
                           suite :suite
                           test-nm :test
                           thr-idx :thr-idx
                           ops :ops
                           :as opts}]
  (if-let [test (-> (zen/get-symbol ztx test-nm)
                    (fix-symbols ))]
    (test-init ztx (assoc opts :test test :thr-idx thr-idx))
    (throw (Exception. (str "No test " test-nm)))))

(take 5 (cycle  [1 2 3]))

(defn longest-test [ztx tests]
  (->> tests
       (map (fn [t] (-> (zen/get-symbol ztx t)
                        (fix-symbols)
                        :steps
                        count)))
       (apply max)))




(defn run-suite-combined [ztx {subj :subj  suite-nm :suite :as opts} suite metrics]
  (into {}
        (for [num-thr (or (:threads suite) [4])]
          [num-thr
           (let [stat
                 @(second
                   (tufte/profiled {:id num-thr
                                    :dynamic? true}
                                   (let [start (start-time)
                                         lbls {:subject (name (:zen/name subj))
                                               :suite (str (name suite-nm))
                                               :threads num-thr}]
                                     (println "\n" "run combined" (or (:zen/desc suite) "") :threads num-thr
                                              (if (:ops suite)
                                                (str "ops:" (:ops suite))
                                                (str "duration: " (:duration-sec suite) "sec.")))
                                     (->> (range num-thr)
                                          (mapv (fn [thr-idx]
                                                  (future
                                                    (loop [i (or (:ops suite) 1)
                                                           n 1]
                                                      (when (or (when (:duration-sec suite)
                                                                  (< (System/currentTimeMillis)
                                                                     (+ start (* 1000 (:duration-sec suite)))))
                                                                (pos? i))
                                                        (doseq [[li test-nm] (map-indexed vector (shuffle (:tests suite)))]
                                                          (run-test ztx (assoc opts
                                                                               :ops (or (:ops suite) 1)
                                                                               :step (* (inc li) n)
                                                                               :longest-test (longest-test ztx (:tests suite))
                                                                               :suite suite
                                                                               :start start
                                                                               :test-nm test-nm
                                                                               :test test-nm
                                                                               :labels lbls
                                                                               :thr-idx thr-idx
                                                                               :thr-max num-thr)))
                                                        (recur (dec i) (inc n)))))))
                                          (mapv deref))
                                     (let [d (measure-time start)]
                                       (prometheus.core/gauge metrics :suite_duration lbls d)
                                       (println (str "\nDone in " (measure-time start) " seconds"))))))]
             (println "\nFinished combined" ":" num-thr)
             (println (tufte/format-pstats stat))
             stat)])))

(defn run-suite-per-test [ztx {subj :subj  suite-nm :suite :as opts} suite metrics]
  (into {}
        (for [test-nm (shuffle (:tests suite))]
          [test-nm
           (into {}
                 (for [num-threads (or (:threads suite) [4])]
                   [num-threads
                    (let [stat
                          @(second
                            (tufte/profiled
                             {:id [test-nm num-threads]
                              :dynamic? true}
                             (let [start (start-time)
                                   lbls {:subject (name (:zen/name subj))
                                         :suite (str (name suite-nm))
                                         :threads num-threads}]
                               (println "\n" "test-name:" test-nm :run-suite suite-nm (or (:zen/desc suite) "") :threads num-threads
                                        (if (:ops suite)
                                          (str "ops:" (:ops suite))
                                          (str "duration: " (:duration-sec suite) "sec.")))
                               (->> (range num-threads)
                                    (mapv (fn [thr-idx]
                                            (future
                                              (loop [i (or (:ops suite) 1)
                                                     n 1]
                                                (when (or (when (:duration-sec suite)
                                                            (< (System/currentTimeMillis)
                                                               (+ start (* 1000 (:duration-sec suite)))))
                                                          (pos? i))
                                                  (run-test ztx (assoc opts
                                                                       :ops (or (:ops suite) 1)
                                                                       :suite suite
                                                                       :step n
                                                                       :start start
                                                                       :test-nm test-nm
                                                                       :test test-nm
                                                                       :labels lbls
                                                                       :thr-idx thr-idx
                                                                       :thr-max num-threads))
                                                  (recur (dec i) (inc n)))))))
                                    (mapv deref)))))]
                      (println "\nFinished " test-nm ":" num-threads)
                      (println (tufte/format-pstats stat))
                      stat)]))])))

(defn run-suite [ztx {subj :subj  suite-nm :suite :as opts}]
  (let [metrics (:metrics @ztx)]
    (if-let [suite (zen/get-symbol ztx suite-nm)]
      (do
        (when (:init suite)
          (print "Test init ")
          (test-init ztx (assoc opts
                                :test {:init (:init suite)}
                                :suite (dissoc suite :common-params)
                                :thr-max 1
                                :thr-idx 1))
          (doseq [test-nm (shuffle (:tests suite))]
            (run-test-inits ztx (assoc opts
                                       :test test-nm
                                       :suite (dissoc suite :common-params)
                                       :thr-max 1
                                       :thr-idx 1)))
          )
        [(run-suite-combined ztx opts suite metrics)
         (run-suite-per-test ztx opts suite metrics)])
      (throw (Exception. (str "No suite " suite-nm))))))

(def tables-sql #_"from: https://wiki.postgresql.org/wiki/Disk_Usage" "
WITH RECURSIVE pg_inherit(inhrelid, inhparent) AS
    (select inhrelid, inhparent
    FROM pg_inherits
    UNION
    SELECT child.inhrelid, parent.inhparent
    FROM pg_inherit child, pg_inherits parent
    WHERE child.inhparent = parent.inhrelid),
pg_inherit_short AS (SELECT * FROM pg_inherit WHERE inhparent NOT IN (SELECT inhrelid FROM pg_inherit))
SELECT table_schema
    , TABLE_NAME AS table
    , row_estimate AS rows
    , total_bytes AS total
    , index_bytes AS index
    , toast_bytes AS toast
    , table_bytes AS size
  FROM (
    SELECT *, total_bytes-index_bytes-COALESCE(toast_bytes,0) AS table_bytes
    FROM (
         SELECT c.oid
              , nspname AS table_schema
              , relname AS TABLE_NAME
              , SUM(c.reltuples) OVER (partition BY parent) AS row_estimate
              , SUM(pg_total_relation_size(c.oid)) OVER (partition BY parent) AS total_bytes
              , SUM(pg_indexes_size(c.oid)) OVER (partition BY parent) AS index_bytes
              , SUM(pg_total_relation_size(reltoastrelid)) OVER (partition BY parent) AS toast_bytes
              , parent
          FROM (
                SELECT pg_class.oid
                    , reltuples
                    , relname
                    , relnamespace
                    , pg_class.reltoastrelid
                    , COALESCE(inhparent, pg_class.oid) parent
                FROM pg_class
                    LEFT JOIN pg_inherit_short ON inhrelid = oid
                WHERE relkind IN ('r', 'p')
             ) c
             LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
  ) a
  WHERE oid = parent
) a
ORDER BY total_bytes DESC;
")

(defn collect-pg-data
  "returns
  {:table-name {:total size-in-bytes
                :index size-in-bytes
                :toast size-in-bytes
                :size size-in-bytes
                :rows rows-estimate}}"
  [ztx opts]
  (let [suite (zen/get-symbol ztx (:suite opts))
        subject (:subj opts)
        resource-types (into {} (map (fn [test-symbol] [(str/lower-case (name test-symbol)) (name test-symbol)]) (:tests suite)))]
    @(http/request
      (merge (cond-> {:url (str (:url subject) "/$sql")
                      :method :post
                      :headers {"content-type" "application/json"}
                      :body (cheshire.core/generate-string [tables-sql])}
               (:basic-auth subject) (assoc :basic-auth (:basic-auth subject))))
      (fn [{st :status ex :error :as res}]
        (if (or #_(:debug req) ex (= (:status res) 500))
          (println ::error)
          (let [all-tables-info (cheshire.core/parse-string (:body res) keyword)
                tables-info (filter #(resource-types (:table %)) all-tables-info)]
            tables-info
            (reduce (fn [acc table-info]
                      (assoc acc (keyword (resource-types (:table table-info)))
                             (dissoc table-info :table :table_schema))) {} tables-info)))))))


(defn collect-test [suite test]
  (for [step (:steps test)]
    (let [method (str/upper-case (name (:method step)))
          uri (:uri step)
          params (str/join "&" (map #(str (name %) "=%") (keys (:params step))))
          common-params (str/join "&" (map (fn [[param-name param-value]] (str (name param-name) "=" (str param-value))) (:common-params suite)))
          all-params (str/join "&" [params common-params])]
      {:request (format "%s %s?%s" method uri all-params)
       :name (:action step)})))

(defn collect-requests [ztx opts]
  (let [suite (zen/get-symbol ztx (:suite opts))
        tests (:tests suite)]
    (reduce (fn [data test-symbol]
              (let [test-name (name test-symbol)
                    test (zen/get-symbol ztx test-symbol)]
                (assoc data test-name
                       (collect-test suite test))))
            {}
            tests)))


(defn bytes->human-readable [bytes & [si?]]
  (let [unit (if si? 1000 1024)]
    (if (< bytes unit) (str bytes " B")
        (let [exp (int  (/ (Math/log bytes)
                           (Math/log unit)))
              pre (str (nth (if si? "kMGTPE" "KMGTPE") (dec exp)) (if-not si? "i" ))]
          (format "%.1f %sB" (/ bytes (Math/pow unit exp)) pre)))))


(defn render-table-stats [stats table-name]
  (str "#### Table statistics\n"
       "- Table size: " (bytes->human-readable (get-in stats [table-name :size])) "\n"
       "- Index size: " (bytes->human-readable (get-in stats [table-name :index])) "\n"
       "- Rows: " (format "%,d" (int (get-in stats [table-name :rows]))) "\n"))

(defn render-query-report [request-stats resource-type]
  (str "#### Queries\n"
       (str/join ""
                 (for [request (get request-stats resource-type)]
                   (str "- " (:name request)
                        ": `" (:request request) "`\n")))))

(defn time-format [ns]
  (cond
    (>= ns 6e10) (str (format "%.2f" (/ ns 6e10)) "m ")
    (>= ns 1e9)  (str (format "%.2f" (/ ns 1e9))  "s ")
    (>= ns 1e6)  (str (format "%.2f" (/ ns 1e6))  "ms")
    (>= ns 1e3)  (str (format "%.2f" (/ ns 1e3))  "μs")
    :else        (str (format "%.2f"    ns)       "ns")))


(defn mxcount [ss]
  (apply mapv
         (fn [& args]
           (apply max (map #(count (str %)) args)))
         ss))

(mxcount
'(["threads" "n" "min" "mean" "p50" "p99" "sum"] [1 4 "227.91ms" "261.00ms" "248.96ms" "337.41ms" "1.04s"])
 )

(defn indent
  [v ln sep]
  (str sep sep
       v
       (str/join "" (repeat (- ln (count (str v))) sep))
       sep sep))


(defn lnormilize
  ([length] (lnormilize length (map (constantly "") length) "-"))
  ([length row sep]
   (map-indexed (fn [idx val]
                  (let [ln (nth length idx)]
                    (indent val ln sep))) row)))

(defn format-table [ss]
  (let [max-length (mxcount ss)
        [head & dts] ss]
    (str "|" (str/join "|" (lnormilize max-length head " ")) "|" "\n"
         "|" (str/join "|" (lnormilize max-length)) "|" "\n"
         (->> dts
              (mapv (fn [dt]
                      (str "|" (str/join "|" (lnormilize max-length dt " ")) "|" )))
              (str/join "\n")))))

(defn render-performance-report [suite stats]
  (str "### Performance statistics"
       "\n"
       "#### Throughput\n"
       (format-table
        (concat
         [["threads" "RPS avg" "nCalls" "min" "mean" "p50" "p99" "t. sum" "t. actual" ]]
         (->> (sort-by first stats)
              (map (fn [[threads {{data :combined} :stats
                                  {total :total} :clock }]]
                     [threads
                      (format "%.1f" (/ (:n data) (/ total 1000000000.)))
                      (:n data)
                      (time-format (:min data))
                      (time-format (:mean data))
                      (time-format (:p50 data))
                      (time-format (:p99 data))
                      (time-format (:sum data))
                      (time-format total)])))))
       "\n\n"

       (->> (for [[threads data'] stats
                  :let [data (dissoc (:stats data') :combined)
                        clock (:clock data')]]
              (str "#### " threads " threads\n"
                   "Actual duration: " (time-format (:total clock)) "\n"
                   (format-table
                    (concat
                     [["threads" "nCalls"  "p50" "p99" "min" "mean" "sum"]]
                     (->> (sort-by (comp :p50 second) data)
                          (remove #(= :tufte/compaction (first %)))
                          reverse
                          (map (fn [[request stat :as rr]]
                                 [request
                                  (:n stat)
                                  (time-format (:p50 stat))
                                  (time-format (:p99 stat))
                                  (time-format (:min stat))
                                  (time-format (:mean stat))
                                  (time-format (:sum stat))])))))
                   "\n"))
            (str/join "\n"))
       "\n\n"))

(defn render-report [{:keys [suite pg-stats request-stats per-test-stats combined-stats]}]
  (str "\n"
       "# Performance test report\n"
       "## Combined (shuffled queries to all resources)\n\n"
       (render-performance-report suite combined-stats)
       "## By resource type\n"
       (str/join "\n"
                 (for [test-symbol (:tests suite)
                       :let [test-name (name test-symbol)]]
                   (str "### " test-name "\n"
                        (render-table-stats pg-stats (keyword test-name))
                        (render-query-report request-stats test-name)
                        (render-performance-report suite
                                                   (per-test-stats test-symbol)))))
       "\n\n"))

(defn render-report-from-edn [path]
  (println (render-report (clojure.edn/read-string (slurp path))))
  )

(comment
  (render-report-from-edn "report-2022-02-14T13:29:34.edn")

  )

(defn run-subject [ztx {subj :subj :as opts}]
  (println :run (:zen/desc subj) (:url subj) (or (:prefix subj) ""))
  (doseq [suit (:suites subj)]
    (let [[combined-stats per-test-stats] (run-suite ztx (assoc opts :suite suit))
          request-stats (collect-requests ztx (assoc opts :suite suit))
          pg-stats (collect-pg-data ztx (assoc opts :suite suit))
          suite (zen/get-symbol ztx suit)
          stat-data {:combined-stats combined-stats
                     :per-test-stats per-test-stats
                     :pg-stats pg-stats
                     :request-stats request-stats
                     :suite suite}
          report-md (render-report stat-data)
          t (subs (str (java.time.LocalDateTime/now)) 0 19)]
      (spit (format "report-%s.edn" t) (with-out-str (clojure.pprint/pprint stat-data)))
      (spit (format "report-%s.md" t) report-md)
      (println report-md))))

(defn run [ztx entrypoint]
  (let [ns (namespace entrypoint)]
    (zen/read-ns ztx (symbol ns))
    ;; (println (zen/errors ztx))
    (if-let [subj (zen/get-symbol ztx entrypoint)]
      (let [subj-report (run-subject ztx {:subj subj})]
        subj-report)
      (throw (Exception. (str "Could not find " entrypoint))))))

(defn metrics-job [ztx]
  (let [metrics (:metrics @ztx)]
    (future
      (loop [i 0]
        (when-not (:stop/metrics @ztx)
          (try
            (let [resp @(http/post "http://localhost:9091/metrics/job/perf-test/instance/run"
                                   {:body (prometheus.core/serialize metrics)})]
              (print (if (= 200 (:status resp)) (str  i "*")"!"))
              (flush))
            (catch Exception e
              (println :error e)))
          (Thread/sleep 1000)
          (recur (inc i)))))))


(defn -main [suite url basic-user basic-password & [file]]
  (let [metrics (prometheus.core/new-registry)
        ztx (zen/new-context {:metrics metrics})
        suite-sym (symbol suite)
        suite-ns-sym (symbol (namespace suite-sym))
        ns (-> '{ns perf
                 server {:zen/tags #{perfbox/subject}
                         :zen/desc "Test server"}}
               (assoc-in ['server :url] url)
               (assoc-in ['server :suites] #{suite-sym})
               (assoc-in ['server :basic-auth] [basic-user basic-password]))]
    (if file
      (zen.store/load-ns ztx (clojure.edn/read-string (slurp file)))
      (zen.core/read-ns ztx suite-ns-sym))
    (do (metrics-job ztx)
        (zen.core/load-ns ztx ns)
        (run ztx 'perf/server)))
  (Thread/sleep 1200)
  (System/exit 0))

(comment
  (def metrics (prometheus.core/new-registry))

  (def ztx (zen/new-context {:metrics metrics}))


  (zen.store/load-ns ztx (clojure.edn/read-string (slurp "/tmp/invc.edn")))

  (zen.store/load-ns ztx (clojure.edn/read-string (slurp "/tmp/invc.edn")))

  (metrics-job ztx)

  (-main "invc/one-thread" "http://localhost:8765" "root" "secret" "/tmp/invc.edn")

  (-main "perfbox.innovaccer/debug" "http://aidbox.innovacer.aidbox.io" "root" "secret")

  (zen.core/read-ns ztx 'invc);; => :zen/loaded

  (zen.core/load-ns ztx '{ns perf
                          server {:zen/tags #{perfbox/subject}
                                  :zen/desc "Test server"
                                  :suites #{perfbox.innovaccer/one-thread}
                                  :url "http://localhost:8765"
                                  :basic-auth ["root" "secret"]}})

  (swap! ztx assoc :stop/metrics true)
  (swap! ztx assoc :stop/metrics false)

  (get-in @ztx [:persist-store :persist/patient-id])

  (zen.core/read-ns ztx 'perfbox.innovaccer)
  (run ztx 'perf/server)

  (run ztx 'perfbox.innovaccer/suite-1)

  (println (str/join "\n" (prometheus.core/get-metric metrics :suite_duration)))

  (println (prometheus.core/serialize metrics))



  ;; (reduce + (vals (prometheus.core/get-metric ztx :request_count)))


  )
