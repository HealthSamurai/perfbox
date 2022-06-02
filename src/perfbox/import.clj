;; test imports
(ns perfbox.import
  (:require
   [org.httpkit.client :as http]
   [zen.core :as zen]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cheshire.core]
   [clj-time.core :as time]
   [clj-time.format :as time-format]
   [clojure.java.shell :as shell]
   [prometheus.core])
  (:import [java.util UUID]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.util.zip GZIPInputStream]))

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

(defn request [ztx subj req]
  (let [metrics (:metrics @ztx)
        lbls (:labels req)
        url (str (:url subj) (:prefix subj) (:uri req))
        body (when-let [d (:data req)] (if (string? d) d (cheshire.core/generate-string d)))
        action (or (:action req) (str (name (or (:method req) :get)) (:uri req)))
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

(defn get-server [ztx srv-id]
  (if-let [srv (get-in @ztx [:servers srv-id])]
    (assoc srv :id srv-id)
    (throw (Exception.  (str "No server " srv-id ", " (keys (:servers @ztx)))))))

(defn check [ztx srv]
  (let [cfg (get-server ztx srv)]
    (if (= 200 (:status @(request ztx cfg {:method :get :uri "/Patient?_count=1&_total=none"})))
      :ok
      (throw (Exception.  (str "cant access " cfg))))))

(defn *request [ztx srv req]
  (let [cfg (get-server ztx srv)]
    (request ztx cfg req)))

(defmulti mk-params (fn [ztx srv test] (:type test)))
(defmulti do-op        (fn [ztx srv test params] (:type test)))

(defn run-op [ztx srv test]
  (let [cfg (get-server ztx srv)
        params (mk-params ztx cfg test)
        res (do-op ztx cfg test (assoc params :labels {:server (name srv)
                                                       :threads 1
                                                       :test (str (:id test))}
                                       :i 0))]
    (-> res
        (dissoc :opts)
        (assoc :body (when-let [b (:body res)]
                       (if (string? b) b (slurp b)))))))

(defn bench [ztx srv test]
  (if (:probe test)
    (run-op ztx srv test)
    (do
      (assert (:threads test) "Need :threads [2 4 ] in test")
      (assert (:duration test) "Need :duration 30 sec in test")
      (ensure-metrics-job ztx)
      (future
        (when-not (:tests/stop @ztx)
          (let [cfg (get-server ztx srv)
                thr (:threads test)
                lbls (merge (:labels test)
                            (:labels srv)
                            {:server  (name srv)
                             :test    (name (:id test))})
                params (mk-params ztx cfg test)]
            (println )
            (println :bench srv)
            ;; (println "  " :test test)
            (doseq [thr (:threads test)]
              (when-not (:tests/stop @ztx)
                (let [lbls (assoc lbls :threads thr)]
                  (println :lbls lbls)
                  (run-threads ztx thr (:duration test)
                               (fn [thr i]
                                 (do-op ztx cfg test (assoc params :labels lbls :i i)))))))))))))


(defn bench! [ztx srv test]
  (if (:probe test)
    (run-op ztx srv test)
    (do 
      (assert (:upto test) "Need :upto 20")
      (ensure-metrics-job ztx)
      (when-not (:tests/stop @ztx)
        (future
          (let [cfg (get-server ztx srv)
                stop (atom false)
                upto (:upto test)
                duration (or (:duration test) 60)
                metrics (:metrics @ztx)
                lbls (merge (:labels test)
                            (:labels srv)
                            {:server  (name srv)
                             :test    (name (:id test))})
                params (mk-params ztx cfg test)]
            (println )
            (println :bench srv)
            (println "  " :test test)
            (loop [i 1]
              (if (or (> i upto) (:tests/stop @ztx) @stop)
                (do (reset! stop true)
                    (println "Done " upto))
                (do
                  (prometheus.core/gauge metrics :perf_threads lbls i)
                  (-> (Thread.
                       (fn []
                         (print (str "{" i "}")) (flush)
                         (loop [i 0]
                           (do-op ztx cfg test (assoc params :labels lbls :i i))
                           (when (and (not (:tests/stop @ztx)) (not @stop))
                             (recur (inc i))))))
                      (.start))
                  (Thread/sleep (* duration 1000))
                  (recur (inc i)))))))))))


(defn reload-config [ztx config]
  (swap! ztx merge config)
  :ok)




(defmethod do-op
  :default
  [ztx srv _test request-params]
  (dissoc @(request ztx srv request-params) :opts))

(defmethod mk-params
  :request
  [ztx srv {req :request}]
  req)

(defmethod mk-params
  :create
  [ztx srv {{rt :resourceType data :data} :resource}]
  {:method :post
   :uri (str "/" rt)
   ;; :headers {"prefer" "return=minimal"}
   :data (cond
           data data
           (= "Patient" rt)
           (read-gz "perfbox/pt.json.gz")
           (= "ExplanationOfBenefit" rt)
           (read-gz "perfbox/eob.json.gz")
           :else
           (throw (Exception. (str "Unknown " rt))))})

(defn dyn-enc [i]
  {:appointment [{:display (str "Appointment/example i")}],
     :diagnosis
     [{:condition {:display "Condition/stroke"},
       :use
       {:coding
        [{:system "http://terminology.hl7.org/CodeSystem/diagnosis-role",
          :code "AD",
          :display "Admission diagnosis"}]},
       :rank 1}
      {:condition {:display "Condition/f201"},
       :use
       {:coding
        [{:system "http://terminology.hl7.org/CodeSystem/diagnosis-role",
          :code "DD",
          :display "Discharge diagnosis"}]}}],
     :serviceProvider {:display "Organization/2"},
     :episodeOfCare [{:display "EpisodeOfCare/example"}],
     :type
     [{:coding
       [{:system "http://snomed.info/sct",
         :code "183807002",
         :display "Inpatient stay for nine days"}]}],
     :participant
     [{:type
       [{:coding
         [{:system
           "http://terminology.hl7.org/CodeSystem/v3-ParticipationType",
           :code "PART"}]}],
       }],
     :resourceType "Encounter",
     :account [{:display "Account/example"}],
     :priority
     {:coding
      [{:system "http://snomed.info/sct",
        :code "394849002",
        :display "High priority"}]},
     :status "planned",
     :class
     {:system "http://terminology.hl7.org/CodeSystem/v3-ActCode",
      :code "IMP",
      :display "inpatient encounter"},
   :identifier [{:use "temp", :value (str "id-" i)}],
     :hospitalization
     {:origin {:display "Location/2"},
      :admitSource
      {:coding
       [{:system "http://snomed.info/sct",
         :code "309902002",
         :display "Clinical Oncology Department"}]},
      :reAdmission {:coding [{:display "readmitted"}]},
      :dietPreference
      [{:coding
        [{:system "http://snomed.info/sct",
          :code "276026009",
          :display "Fluid balance regulation"}]}],
      :specialCourtesy
      [{:coding
        [{:system
          "http://terminology.hl7.org/CodeSystem/v3-EncounterSpecialCourtesy",
          :code "NRM",
          :display "normal courtesy"}]}],
      :specialArrangement
      [{:coding
        [{:system
          "http://terminology.hl7.org/CodeSystem/encounter-special-arrangements",
          :code "wheel",
          :display "Wheelchair"}]}],
      :destination {:display "Location/2"}},
     :basedOn [{:display "ServiceRequest/myringotomy"}],
     :partOf {:display "Encounter/f203"},
     :subject {:display "Patient/f201"},
   :statusHistory [{:status "in-progress", :period {:start "2013-03-08"}}]}

  )
(defmethod mk-params
  :dyn-enc
  [ztx srv params]
  {})

(defmethod do-op
  :dyn-enc
  [ztx srv _test request-params]
  (dissoc @(request ztx srv
                    {:uri "/Encounter"
                     :method :post
                     :labels (:labels request-params)
                     :data (dyn-enc (:i request-params))})))

(defmethod mk-params
  :transaction
  [ztx srv {rt :resourceType}]
  (println :server srv)
  (println :init-1 (:status @(request ztx srv {:method :post :uri (str "/") :data (read-gz "perfbox/init-1.json.gz")})))
  (println :init-2 (:status @(request ztx srv {:method :post :uri (str "/") :data (read-gz "perfbox/init-2.json.gz")})))
  (let [bundle (read-gz "perfbox/transaction-1.json.gz")]
    {:method :post
     :uri (str "/")
     :data bundle})
  )

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




 (def enc2
   (cheshire.core/generate-string
    {:appointment [{:display "Appointment/example"}],
     :diagnosis
     [{:condition {:display "Condition/stroke"},
       :use
       {:coding
        [{:system "http://terminology.hl7.org/CodeSystem/diagnosis-role",
          :code "AD",
          :display "Admission diagnosis"}]},
       :rank 1}
      {:condition {:display "Condition/f201"},
       :use
       {:coding
        [{:system "http://terminology.hl7.org/CodeSystem/diagnosis-role",
          :code "DD",
          :display "Discharge diagnosis"}]}}],
     :serviceProvider {:display "Organization/2"},
     :episodeOfCare [{:display "EpisodeOfCare/example"}],
     :type
     [{:coding
       [{:system "http://snomed.info/sct",
         :code "183807002",
         :display "Inpatient stay for nine days"}]}],
     :participant
     [{:type
       [{:coding
         [{:system
           "http://terminology.hl7.org/CodeSystem/v3-ParticipationType",
           :code "PART"}]}],
       }],
     :resourceType "Encounter",
     :account [{:display "Account/example"}],
     :priority
     {:coding
      [{:system "http://snomed.info/sct",
        :code "394849002",
        :display "High priority"}]},
     :status "planned",
     :class
     {:system "http://terminology.hl7.org/CodeSystem/v3-ActCode",
      :code "IMP",
      :display "inpatient encounter"},
     :identifier [{:use "temp", :value "Encounter_Roel_20130311"}],
     :hospitalization
     {:origin {:display "Location/2"},
      :admitSource
      {:coding
       [{:system "http://snomed.info/sct",
         :code "309902002",
         :display "Clinical Oncology Department"}]},
      :reAdmission {:coding [{:display "readmitted"}]},
      :dietPreference
      [{:coding
        [{:system "http://snomed.info/sct",
          :code "276026009",
          :display "Fluid balance regulation"}]}],
      :specialCourtesy
      [{:coding
        [{:system
          "http://terminology.hl7.org/CodeSystem/v3-EncounterSpecialCourtesy",
          :code "NRM",
          :display "normal courtesy"}]}],
      :specialArrangement
      [{:coding
        [{:system
          "http://terminology.hl7.org/CodeSystem/encounter-special-arrangements",
          :code "wheel",
          :display "Wheelchair"}]}],
      :destination {:display "Location/2"}},
     :basedOn [{:display "ServiceRequest/myringotomy"}],
     :partOf {:display "Encounter/f203"},
     :subject {:display "Patient/f201"},
     :statusHistory
     [{:status "in-progress", :period {:start "2013-03-08"}}],}))

(def hetzner-config
  {:metrics/push-url "http://s1-fi.health-samurai.io:9091/metrics/job/perf-test/instance/run"
   :prometheus/url "http://s1-fi.health-samurai.io:9090"
   :servers
   {:aidbox {:url "http://10.96.226.88:8765/fhir"
             :basic-auth ["root" "secret"]}

    :httpit {:url "http://10.96.226.88:5001"}

    :jetty {:url "http://10.96.226.88:5002"}

    :nginx {:url "http://localhost"}

    :immutant {:url "http://10.96.226.88:5003"}

    :aidboxx {:url "http://10.96.226.88:8765"
              :basic-auth ["root" "secret"]}

    :twobox {:url "http://10.96.226.88:8888/fhir"
             :basic-auth ["root" "secret"]}

    :twoboxx {:url "http://10.96.226.88:8888"
              :basic-auth ["root" "secret"]}

    :hapi {:url "http://10.96.226.46:8888/fhir"}

    :kefhir {:url "http://localhost:8181/fhir"}

    :ibm {:url "https://10.96.226.221:9443/fhir-server/api/v4"
          :basic-auth ["fhiruser" "admin"]}}})

(defn init-hetzner []
  (def metrics (prometheus.core/new-registry))
  (send metrics assoc-in [:meta :perf_request_duration :buckets] [0.00001 0.00002 0.00005 0.0001 0.0002 0.0005 0.001 0.002 0.003 0.004 0.005 0.006 0.007 0.008 0.009 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.1 0.2 0.3 0.4 0.5 1 2 3 4 5 10])

  
  (def ztx (zen/new-context {:metrics metrics}))

  (reload-config ztx hetzner-config)


  (check-metrics ztx)
  (start-metrics-job ztx))



(defn aidbox-pids []
  (->> (str/split (:out (shell/sh "jps")) #"\n")
       (filter (fn [x] (str/includes? x "aidbox")))
       (mapv #(str/split % #"\s+"))
       (mapv first)))


(defn profile-aidbox [nm]
  (doseq [pid (aidbox-pids)]
    (future
      (let [cmd (format "/root/async-profiler/profiler.sh -e cpu -d 60 -f /var/www/html/%s-%s-%s.html %s"
                        (subs 
                         (.format (java.time.format.DateTimeFormatter/ISO_DATE_TIME)
                                  (java.time.LocalDateTime/now)) 0 16)
                        nm
                        pid
                        pid)]
        (println :$ cmd)
        (let [res (shell/sh "bash" "-c" cmd)]
          (println :res res))))))



(comment
  (comment 
    (stop-metrics-job ztx))

  (init-hetzner)
  (ensure-metrics-job ztx)
  (check ztx :aidbox)
  (check ztx :aidboxx)
  (check ztx :twobox)
  (check ztx :twoboxx)
  (check ztx :hapi)
  (check ztx :ibm)
  (check ztx :kefhir)


  (swap! ztx assoc :tests/stop true)

  (def enc-crud
    {:type :request
     :id :create-enc
     :upto 60
     :threads [60]
     :duration 180
     :request {:method :post
               :uri  "/Encounter"
               ;; :headers {"prefer" "return=minimal"}
               :data enc2}})

  (profile-aidbox "cerate-enc-aidbox-noval")

  (bench ztx :aidbox
          (assoc enc-crud
               :probe true
               :timeout 500000
               :id "create-enc-new-opt"))

  (def srv :aidbox)

  (do
    (println :init-1 (:status @(*request ztx srv {:method :post :uri (str "/") :data (read-gz "perfbox/init-1.json.gz")})))
    (println :init-2 (:status @(*request ztx srv {:method :post :uri (str "/") :data (read-gz "perfbox/init-2.json.gz")})))
    )

  (def batch (assoc (cheshire.core/parse-string (read-gz "perfbox/transaction-1.json.gz") keyword) :type "batch"))

  batch

  (def tx
    {:type :request
     :id :create-enc
     :upto 40
     :threads [40]
     :duration 180
     :request {:method :post
               :timeout 100000
               :uri  "/"
               :headers {"prefer" "return=minimal"}
               :data (read-gz "perfbox/transaction-1.json.gz")}})

  (bench ztx srv
         (assoc tx
;;              :probe true
               :id "tx"))


 (prom-query ztx "avg(rate(perf_request_count[1m])) by (server,test,threads)"
             (time/minus (time/now) (time/seconds 60))
             (time/now))


 (prom-query ztx "max(rate(perf_request_count[1m])) by (server,test,threads)"
             (time/minus (time/now) (time/seconds 60))
             (time/now))

 (defn spath [x] (str "file:///data/fhir/" x))

 "Observation" "ExplanationOfBenefit" "Encounter"

 (doseq  [rt ["Observation"]]
   (future
     (println :start rt)
     (def resp
       (time 
        @(*request ztx :aidboxx
                   {:method :post
                    :timeout 10000000
                    :uri (str "/" rt "/$load")
                    :data {:source (spath (str rt ".ndjson.gz"))}})))
     (println :done rt resp)))

 ;; (/ 62.4 34)
 ;; (/ 1200.0 34)
 ;; (/ 11.8 2.4)
 ;; (/ 624.0 436.4)
 ;; (/ 3000.0 161.0)

 ;; 1.4K rps EOB
 ;; 1.8K rps ENC
 ;; 4.9 rps PAT
 ;; 18.6 rps OBS (with import) => 30-40K
 ;; 14MB/s

 ;; 35 MB/s

 ;; resp

 )

(comment

  (def res
    @(request ztx aidbox {}
              {:method :post :uri (str "/$import")
               :data {:id "pt"
                      :inputFormat "application/fhir+ndjson",
                      :contentEncoding "gzip",
                      :mode "bulk",
                      :storageDetail {:type "file"}
                      :inputs
                      [{:resourceType "Patient",
                        :url "/Users/niquola/synthea/output/fhir/Patient.ndjson.gz"}]}}))


  


  (doseq [rt ["AllergyIntolerance"
              "Device"
              "Medication"
              "Patient"
              "MedicationAdministration"]]
    (future
      (def resp
        @(request ztx aidbox {}
                  {:method :post
                   :uri (str "/" rt "/$load")
                   :data {:source (str "file:///Users/niquola/synthea/output/fhir/" rt ".ndjson.gz")}}))
      (println :done resp)))

  (get-body (request ztx aidbox {} {:method :get :uri (str "/BulkImportStatus/pt")}))
  
  
  (prometheus.core/gauge metrics :perf_threads {} 1)

  )
