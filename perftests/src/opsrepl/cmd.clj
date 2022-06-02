(ns opsrepl.cmd
  (:require
   [clojure.java.shell :refer [sh] :as shell]
   [cheshire.core :as json]
   [clj-yaml.core :as yaml]
   [clojure.string :as str])
  (:import [java.io File]))

(defn u [& args]
  (->> args
       (reduce (fn [acc x]
                 (if (nil? x)
                   acc
                   (conj acc
                         (cond
                           (vector? x)
                           (apply u x)

                           (or (keyword? x) (string? x) (symbol? x))
                           (name x)

                           (map? x)
                           (->> x
                                (mapv (fn [[k v]]
                                        (str "-" (name k)
                                             (when-not (= true v)
                                               (str " " v)))))
                                (str/join " "))
                           :else (str x)))))
               [])
       (str/join " ")))

(defn env [m]
  (->> 
   m
   (mapv (fn [[k v]] (str (name k) "='" v "'")))
   (str/join " ")))

(defn lns [s]
  (str/split s #"\n"))

(defn split-cols [l]
  (str/split l #"\s+"))

(defn tbl [s]
  (let [lns (lns s) 
        cols (mapv keyword (split-cols (first lns)))]
    (->> (rest lns)
         (mapv (fn [x] (zipmap cols (split-cols x)))))))

(defn proc-res [res]
  (if (= (:exit res) 0)
    (:out res)
    (do 
      (println :Error)
      (println :cmd (:cmd res))
      (println :out (:out res))
      (println :error (:err res))
      (throw (Exception. (:cmd res))))))

(defn to-yaml [x]
  (clj-yaml.core/generate-string x))

(defn from-yaml [x]
  (clj-yaml.core/parse-string x))

(defn to-json [x]
  (cheshire.core/generate-string x))

(defn from-json [x]
  (cheshire.core/parse-string x keyword))

(defn $> [cmd & [opts]]
  (let [cmd* (u cmd)
        res (clojure.java.shell/sh "bash" "-c" cmd*)]
    (proc-res (assoc res :cmd cmd*))))

(defn tmp-file [& [content]]
  (let [f (File/createTempFile "tmp" "ssh")]
    (when content
      (spit (.getPath f) content))
    (.getPath f)))

(comment


  (u '[ls -lah])

  ($> '[ls -lah "/"])
  
  (tbl ($> '[ps aux]))
  (lns ($> '[ps aux]))

  (def opt {:a true})

  (lns ($> `[docker ps ~opt]))
  (tbl ($> `[docker ps ~opt]))

  )
