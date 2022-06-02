(ns perfbox.synthea
  (:require
   [org.httpkit.client :as http]
   [zen.core :as zen]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cheshire.core]
   [clj-time.core :as time]
   [clj-time.format :as time-format]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [prometheus.core])
  (:import [java.util UUID]
           [java.util.concurrent ConcurrentLinkedQueue]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.util.zip GZIPInputStream]))

(set! *warn-on-reflection* true)

(defn get-files [path]
  (file-seq path))

(defn each-file [{path :dir logs-path :logs num-threads :threads}  hook]
  (let [res (promise)
        fls (.listFiles (io/file path))
        ^ConcurrentLinkedQueue bundles (ConcurrentLinkedQueue.)
        init (->> fls
                  (reduce (fn [acc f]
                            (if (str/includes? (.getName ^java.io.File f) "Information")
                              (conj acc f)
                              (do (.add bundles f)
                                  acc)))
                          []))
        start (System/currentTimeMillis)]
    (println (count fls))
    (future
      (with-open [logw (io/writer logs-path)]
        (doseq [f init]
          (.write logw  (str (str/join "\t" (hook 0 f)) "\n"))
          (.flush logw))
        (->>
         (for [i (range num-threads)]
           (let [^Thread t (Thread. (fn []
                                      (loop []
                                        (when-let [f (.poll bundles)]
                                          (.write logw (str (str/join "\t" (hook i f)) "\n"))
                                          (.flush logw)
                                          (recur)))))]
             (.start t)
             t))
         (mapv (fn [^Thread x] (.join x))))
        (.write logw ^String (str "Done in " (- (System/currentTimeMillis) start) "ms"))
        (.flush logw))
      (deliver res :ok))
    res))

(def path-10k "/data/b10000/fhir")
(def path-100k "/data/b100k/fhir")
(def path-100knd "/data/b100knd/fhir")


(comment
  ;; record request
  ;; record resources count
  ;; record overall time
  ;; record overall speed bundel/sec res/seq

  (def path )

  @(each-file path "/tmp/logs"
              (fn [thread-num f]
                (slurp f)
                (str thread-num "\t" (.getName ^java.io.File f) "\n")))

  )






