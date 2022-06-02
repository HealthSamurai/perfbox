;; test imports
(ns perfbox.common
  (:require
   [org.httpkit.client :as http]
   [zen.core :as zen]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cheshire.core]
   [clojure.java.jdbc :as jdbc]
   [clj-time.core :as time]
   [clj-time.format :as time-format]
   [clojure.java.shell :as shell]
   [prometheus.core])
  (:import [java.util UUID]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.util.zip GZIPInputStream]
           [org.postgresql.jdbc PgArray]
           org.postgresql.util.PGobject
           ))

(set! *warn-on-reflection* true)

;; - 2/4/8/16/32 concurent connections
;; measure - resource (replace id)
;; measure - rps / bps

(def iso-fmt (time-format/formatters :date-time))

(defn time-fmt [d]
  (time-format/unparse iso-fmt d))

(defn start-time []
  (System/nanoTime))


(defn measure-time [start]
  (/ (- (System/nanoTime) start)
     1000000000.0))

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

(defn get-body [req]
  (when-let [b (:body (if (map? req) req @req))]
    (when (string? b)
      (try 
        (cheshire.core/parse-string b keyword)
        (catch Exception e :ups)))))


(defn get-server [ztx srv-id]
  (if-let [srv (get-in @ztx [:servers srv-id])]
    (assoc srv :id srv-id)
    (throw (Exception.  (str "No server " srv-id ", " (keys (:servers @ztx)))))))

(defn request [ztx subj-name {:as req :keys [labels uri data method]}]
  (let [metrics (:metrics @ztx)
        subj (get-server ztx subj-name)
        _ (assert subj)
        action (or (:action req) (str (name (or (:method req) :get)) (:uri req)))
        lbls (assoc (:labels req) :server (name subj-name) :action action)
        url (str (:url subj) (:prefix subj) (:uri req))
        body (when-let [d (:data req)] (if (string? d) d (cheshire.core/generate-string d)))
        req (merge req
                   (cond-> {:url url
                            :method (or (:method req) :get)
                            :timeout  (or (:timeout req) 500)
                            :query-params (merge (:common-params req) (:params req))
                            :headers (merge {"content-type" "application/json"} (:headers req))}
                     (:basic-auth subj) (assoc :basic-auth (:basic-auth subj))
                     body (assoc :body body)))
        start (start-time)]
    (http/request req
     (fn [{st :status ex :error :as res}]
       (prometheus.core/counter metrics :perf_request_count lbls)
       (prometheus.core/histogram metrics :perf_request_duration (assoc lbls :status (or st 500)) (measure-time start))
       (when body
         (prometheus.core/histogram metrics :perf_request_size lbls (* 2 (.length ^String body))))

       (when-let [resp-size (get-in res [:headers :content-length])]
         (prometheus.core/histogram metrics :perf_response_size lbls (Integer/parseInt resp-size)))
       res))))


(defn rpc [ztx srv req]
  (request ztx :aidbox
           {:method :post
            :timeout (:timeout req)
            :uri "/rpc"
            :data (dissoc req :timeout)}))


(defn push-metrics [ztx]
(let [metrics (:metrics @ztx)
        metrics-url (:metrics/push-url @ztx)]
    (assert metrics-url (str "No " :metrics/push-url "is set in ztx"))
    (try
      (:status @(http/post metrics-url {:body (prometheus.core/serialize metrics)}))
      (catch Exception e
        (println :error e)))))

(defn start-metrics-job [ztx]
  (let [stop (atom false)]
    (future
      (println :start-metrics (:metrics/push-url @ztx))
      (loop [i 0]
        (if-not @stop
          (do
            (push-metrics ztx)
            (Thread/sleep 2000)
            (recur (inc i)))
          (println :stop-metrics))))
    (swap! ztx assoc :metrics/push-worker stop)
    stop))


(defn stop-metrics-job [ztx]
  (when-let [stop (:metrics/push-worker @ztx)]
    (reset! stop true)
    (swap! ztx dissoc :metrics/push-worker)
    :stoped)
  :none)

(defn check-metrics [ztx]
  (if (= 200 (:status @(http/post (:metrics/push-url @ztx) {:body ""})))
    :ok
    (throw (Exception. (str "Metrics push sever is" (:metrics/push-url @ztx) " is not accessible")))))

(defn ensure-metrics-job [ztx]
  (check-metrics ztx)
  (if-not (:metrics/push-worker @ztx)
    (start-metrics-job ztx)
    :ok))


(defn read-gz [path]
  (let [s (io/input-stream (io/resource path))
        gz (java.util.zip.GZIPInputStream. s)
        br (java.io.InputStreamReader. gz java.nio.charset.StandardCharsets/UTF_8)]
    (slurp br)))

(defn run-threads [ztx threads duration f]
  (println "\n### threads " threads)
  (let [strt (start-time)]
    (->>
     (for [thr (range threads)]
       (let [th (Thread.
                 (fn []
                   (print (str "{" thr "}"))
                   (flush)
                   (loop [i 0]
                     (f thr i)
                     (if (and (not (:tests/stop @ztx)) (< (measure-time strt) duration))
                       (recur (inc i))
                       (print (str   "\n{" thr " " :i " " i " " :t " " (measure-time strt) "}"))))))]
         (.start th)
         th))
     (mapv identity)
     (mapv (fn [th] (.join ^Thread th))))))


(defn check [ztx srv]
  (let [cfg (get-server ztx srv)]
    (if (= 200 (:status @(request ztx cfg {:method :get :uri "/Patient?_count=1&_total=none"})))
      :ok
      (throw (Exception.  (str "cant access " cfg))))))

(defn *request [ztx srv req]
  (let [cfg (get-server ztx srv)]
    (request ztx cfg req)))

(defn reload-config [ztx config]
  (swap! ztx merge config)
  :ok)

(defn prom-group [body]
  (->> body :data :result
       (reduce (fn [acc {m :metric v :value}]
                 (assoc-in acc [(keyword (:server m)) (keyword (:test m)) (:threads m)] (first v)))
               {})))

(defn prom-query [ztx query & [start stop]]
  (let [prom-url (:prometheus/url @ztx)]
    (let [body (get-body
                (http/request
                 {:url (str prom-url "/api/v1/query_range")
                  :query-params {:query query
                                 :start (time-fmt (or start
                                                      (time/minus (time/now) (time/seconds 60))))
                                 :end   (time-fmt (or stop
                                                      (time/now)))}}))]
      (prom-group body))))


(def config-example
  {:metrics/push-url "http://s1-fi.health-samurai.io:9091/metrics/job/perf-test/instance/run"
   :prometheus/url "http://s1-fi.health-samurai.io:9090"
   :servers
   {:aidbox {:url "http://10.96.226.88:8765/fhir"
             :basic-auth ["root" "secret"]}}})

(defn init [cfg]
  (def metrics (prometheus.core/new-registry))
  (send metrics assoc-in [:meta :perf_request_duration :buckets] [0.00001 0.00002 0.00005 0.0001 0.0002 0.0005 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.1 0.2 0.3 0.4 0.5 1 2 3 4 5 10 15 20 30 40 50 60])
  (let [ztx (zen/new-context {:metrics metrics})]
    (reload-config ztx cfg)
    (check-metrics ztx)
    (start-metrics-job ztx)
    ztx))


(extend-protocol jdbc/IResultSetReadColumn
  PGobject
  (result-set-read-column [pgobj _metadata _index]
    (let [type  (.getType pgobj)
          value (.getValue pgobj)]
      (case type
        "json"      (cheshire.core/parse-string value keyword)
        "jsonb"     (cheshire.core/parse-string value keyword)
        value))))

(def pg-db {:dbtype "postgresql"
            :dbname "devbox"
            :host "10.96.226.65"
            :user "postgres"
            :password "postgres"})

(defonce conn (jdbc/get-connection pg-db))


(defn query [sql]
  (jdbc/query {:connection conn} sql))

(comment

  (System/getenv "PGHOST")

  (query "select id, resource#>'{name,0}' as name, resource->'identifier' as identifiers from patient order by RANDOM() limit 10")


  )
