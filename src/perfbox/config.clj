(ns perfbox.config
  (:require [perfbox.common :as c]
            [com.stuartsierra.frequencies :as freq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [perfbox.synthea :as s]))

(def hetzner
  {:metrics/push-url "http://s1-fi.health-samurai.io:9091/metrics/job/perf-test/instance/run"
   :prometheus/url "http://s1-fi.health-samurai.io:9090"
   :servers
   {:aidbox {:url "http://localhost:8765"
             :basic-auth ["root" "secret"]}

    :hapi {:url "http://10.96.226.46:8888/fhir"}

    :kefhir {:url "http://localhost:8181/fhir"}

    :ibm {:url "https://10.96.226.221:9443/fhir-server/api/v4"
          :basic-auth ["fhiruser" "admin"]}}})

(def ^:dynamic *seed* {})

(defn eval-params [it params]
  (->> params
       (reduce (fn [acc [k v]]
                 (assoc acc k (binding [*seed* it]
                                (eval v))))
               {})))

(defn run-scenario [ztx sc & [opts]]
  (let [report  (atom {})
        sc      (merge sc opts)
        seed    (when-let [sql (:seed sc)] (c/query sql))
        _       (swap! report assoc :seed seed)]
    (future
      (doseq [[sid srch] (:searches sc)]
        (let [test (str (:id sc) "-" sid)]
          (println sid (eval-params (first seed) (:params srch)))
          (doseq [srv (:servers sc)]
            (println :srv srv)
            (doseq [it seed]
              (do (print ".") (flush))
              (let [start (System/currentTimeMillis)
                    params (merge (:params sc) (eval-params it (:params srch)))
                    res  @(c/request
                           ztx srv
                           {:method (or (:method sc) :get)
                            :uri (:uri sc)
                            :timeout (or (:timeout sc) 100000000)
                            :labels {:test test}
                            :params params})]
                (if (and (< (:status res) 300) (> (count (:entry (c/get-body res))) 0))
                  (swap! report update-in [:test sid srv :data] conj (- (System/currentTimeMillis) start))
                  (swap! report update-in [:test sid srv :errors] (fn [x] (inc (or x 0)))))))
            (println)
            ))))
    report))

(def patient-search
  {:id "pt"
   :title "Patient"
   :matrix [1 2 4 8 16 32 64]

   :seed "select
id, resource#>'{name,0}' as name,
resource->'birthDate' as birthDate,
resource->'identifier' as identifier
from patient order by RANDOM() limit 1000"

   :uri "/Patient"
   :method :get
   :timeout 10000000
   :params {:_count 5
            :_total "none"}
   :searches
   {:name           {:params {:name        ['(get-in *seed* [:name :family]) '(first (get-in *seed* [:name :given]))]}}
    :identifier     {:params {:identifier  '(:value (first (:identifier *seed*)))}}
    :identifier-sys {:params {:identifier  '(let [i (first (:identifier *seed*))] (str (:system i) "|" (:value i)))}}
    :birthdate      {:params {:birthdate   '(:birthdate *seed*)}}}})

;; report
#_(doseq [[k tms] @perf-res]
    (let [m (/ (reduce + 0 tms) (* 1.0 (count tms)))
          d  (Math/sqrt (/ (reduce (fn [acc t] (+ acc (* (- t m)(- t m)))) 0 tms)
                           (* 1.0 (count tms))))]
      (println k m "ms +/-" d)))

(comment
  (def ztx (c/init hetzner))

  (c/reload-config ztx hetzner)

  (c/get-server ztx :ibm)

  (def srvs [:aidbox :hapi :ibm])

  (def res (run-scenario ztx patient-search {:servers srvs}))

  (doseq [[tst srvs] (:test @res :seed)]
    (println tst)
    (doseq [[srv {data :data err :errors}]  srvs]
      ;; (println srv)
      ;; (println data)
      (let [res (freq/stats (frequencies data))]
        (println "\t" srv "\t" (:mean res) "\t" (:stdev res) "\t" (or err "ok")))))
  

  (time
   (->
    @(c/request ztx :aidbox
                {:method :get
                 :uri "/Patient"
                 :timeout 100000000
                 :params {:_count 5
                          :birthdate "1880"
                          :_elements "name"
                          :_total "none"}})
    (c/get-body)))

  (c/query "create index patient_bod on patient ((knife_extract_max_timestamptz(resource, '[[\"birthDate\"]]')), (knife_extract_min_timestamptz(resource,'[[\"birthDate\"]]')))")
  (c/query "vacuum analyze patient")


  ;; load 100K pts
  (def load-progress
    (s/each-file
     {:dir s/path-100k :logs "/tmp/ibm10k.logs" :threads 31}
     (fn [thread f]
       (try
         (let [tx (slurp (.getPath f))
               start (System/currentTimeMillis)
               res    @(c/request ztx srv
                                  {:method :post
                                   :timeout 100000
                                   :labels {:test "tx-load"}
                                   :uri  "/"
                                   :headers {"prefer" "return=minimal"}
                                   :data tx})
               d (- (System/currentTimeMillis) start)]
           [thread (.getName f) (:status res) d])
         (catch Exception e
           ["ERROR" (pr-str e)])))))

  ;; use hiccup + vega-light for reports
  ;; we want to generate report
  ;; pg config
  ;; system info
  ;; tests
  ;; test1:
  ;;   number or resources, db size, index size
  ;;  param-test-1:
  ;;    (aidbox, hapi, ibm)
  ;;    thread matrix [1 4 8 16 32 64]: mean+d; percetiles - 3 minutes
  ;;    server - rps; server - latency
  ;;    summary
  ;;  param-test-2:
  ;;    (aidbox, hapi, ibm)


  ;; load with $load
  (->>
   (.listFiles (io/file "/data/b100knd/fhir/"))
   (filter (fn [f] (str/ends-with? (.getName f) ".ndjson.gz")))
   (mapv (fn [f]
           {:name (.getName f)
            :path (.getPath f)
            :rt (str/replace (.getName f) #".ndjson.gz$" "")}))
   ;;(filter (fn [{rt :rt}] (= rt "Patient")))
   (mapv (fn [{rt :rt pth :path nm :name}]
           (future
             (println :start rt)
             (let [resp
                   (time
                    @(c/request ztx :aidbox
                                {:method :post
                                 :timeout 1000000000
                                 :uri (str "/" rt "/$load")
                                 :data {:source pth}}))]
               (println :done rt resp)))))
   )

  (-> 
   @(c/rpc ztx :aidbox
           {:method "aidbox.zen/tagged-symbols"
            :params {:tag "zenbox/rpc"}})
   :body
   (cheshire.core/parse-string keyword))

  (time
   (-> 
    @(c/rpc ztx :aidbox
            {:timeout 1000000000
             :method "aidbox.bulk/load-pjson"
             :params {:file "/data/b100knd/fhir/Encounter.ndjson.gz"
                      :resourceType "Encounter"}})
    :body
    (cheshire.core/parse-string keyword)))


  )
