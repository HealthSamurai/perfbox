(ns perftests
  (:require
   [opsrepl.aws :as aws]
   [opsrepl.ssh :as ssh]
   [clojure.set]
   [opsrepl.cmd :as cmd]))

(defn add-scrapes [old scrapes-idx]
  (let [old-scrapes (:scrape_configs old)
        existing (into #{} (mapv (fn [x] (keyword (:job_name x))) old-scrapes))
        updates  (into #{} (keys scrapes-idx))
        to-add (clojure.set/difference  updates existing)]
    (assoc old :scrape_configs
           (->> old-scrapes
                (mapv (fn [{id :job_name :as job}]
                        (if-let [new (get scrapes-idx (keyword id))]
                          (assoc new :job_name id)
                          job)))
                (into (->> to-add
                           (mapv
                            (fn [id]
                              (let [job (get scrapes-idx id)]
                                (println job)
                                (assoc job :job_name (name id)))))))))))

(defn prepare-db-env
  [ztx]
  (let [opts {:wdir "/home/ubuntu/sansara/box"}]
    (ssh/$> ztx [:ec2 :aidbox-perf-db] `[make up] opts)))

(defn prepare-aidbox-env
  [ztx]
  (let [opts {:source "/home/ubuntu/.sdkman/bin/sdkman-init.sh"
              :wdir "/home/ubuntu/sansara/box"
              :env {:PGHOST "192.168.33.182"}}
        path [:ec2 :aidbox-perf-aidbox]]
    (ssh/$> ztx path '[setsid -f make repl < "/dev/null" > "/tmp/repl-logs" "2>&1"] opts)
    (Thread/sleep 30000)
    (printf "box repl: %s:%s"
            (get-in @ztx [:ssh/agents :ec2 :sessions :aidbox-perf-aidbox :connection :ip])
            (-> (ssh/$> ztx path '[cat ".nrepl-port"] opts)
                (cmd/lns)
                (last)))))

(defn prepare-test-env
  [ztx]
  (let [opts {:source "/home/ubuntu/.sdkman/bin/sdkman-init.sh"
              :wdir "/home/ubuntu/sansara/infrabox"}
        path [:ec2 :aidbox-perf-test]]
    (ssh/$> ztx path '[setsid -f make repl < "/dev/null" > "/tmp/repl-logs" "2>&1"] opts)
    (Thread/sleep 30000)
    (printf "infrabox repl: %s:%s"
            (get-in @ztx [:ssh/agents :ec2 :sessions :aidbox-perf-test :connection :ip])
            (-> (ssh/$> ztx path '[cat ".nrepl-port"] opts)
                (cmd/lns)
                (last)))))


(defn prepare-env
  [ztx insts]
  (ssh/ssh-agent ztx :ec2 {:private-key-path "perf-tests-instance.pem"})
  (doseq [[nm {:keys [ip]}] insts]
    (ssh/session ztx :ec2 nm {:ip ip :user "ubuntu"})
    (case nm
      :aidbox-perf-db
      (prepare-db-env ztx)

      :aidbox-perf-aidbox
      (prepare-aidbox-env ztx)

      :aidbox-perf-test
      (prepare-test-env ztx))))

(comment

  (def ztx (atom {}))
  (def insts (aws/inst-list))

  (prepare-test-env ztx)

  (prepare-aidbox-env ztx)

  (doseq [inst (vals insts)]
    (aws/inst-start inst))

  (doseq [inst (vals insts)]
    (aws/inst-stop inst))

  (ssh/ssh-agent ztx :ec2 {:private-key-path "perf-tests-instance.pem"})

  (doseq [[nm {:keys [ip] :as inst}] insts]
    (ssh/session ztx :ec2 nm {:ip ip :user "ubuntu"}))

  (kill-repl! ztx [:ec2 :aidbox-perf-aidbox])

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[git pull origin master]
           {:wdir "/home/ubuntu/sansara/box"}))

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[git pull origin master]
           {:wdir "/home/ubuntu/sansara/box"}))

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[docker-compose restart ibm]
           {:wdir "/home/ubuntu/sansara/box"
            :env {:PGHOST "192.168.33.182"}}))

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[docker ps]
           {:wdir "/home/ubuntu/sansara/box"
            :env {:PGHOST "192.168.33.182"}}))

  (-> insts :aidbox-perf-aidbox :ip) ;; => "3.231.102.7";; => "44.200.18.97"
  (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
          '[curl -u "fhiruser:admin" -vvv "http://localhost:9080/fhir-server/api/v4/Patient"]
          {:wdir "/home/ubuntu/sansara/box"
           :env {:PGHOST "192.168.33.182"}})

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[docker-compose up -d hapi]
           {:wdir "/home/ubuntu/sansara/box"
            :env {:PGHOST "192.168.33.182"}}))

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-aidbox]
           '[git reset --hard && git pull origin master]
           {:wdir "/home/ubuntu/sansara/box"
            :env {:PGHOST "192.168.33.182"}}))

  (cmd/lns
   (ssh/$> ztx [:ec2 :aidbox-perf-db]
           '[docker-compose logs db]
           {:wdir "/home/ubuntu/sansara/box"
            :env {:PGHOST "192.168.33.182"}}))

  (defn kill-repl!
    [ztx conn]
    (run! (fn [pid] (cmd/lns (ssh/$> ztx conn `[kill -9 ~pid])))
          (cmd/lns (ssh/$> ztx conn '[pgrep java]))))

;;; hz

  (def keypath "/home/kreved/.telega/cache/documents/hetzner-key")
  (ssh/ssh-agent ztx :hz {:private-key-path keypath})
  (ssh/session ztx :hz :fi {:ip "65.108.58.36" :user "root"})

  (def fi [:hz :fi])

  (cmd/lns
   (ssh/$> ztx fi '[ps aux | grep "push"]))

  (ssh/$> ztx fi '[sudo apt install -y prometheus-node-exporter])

  (cmd/lns
   (ssh/$> ztx fi '[ls "/etc/prometheus/prometheus.yml"]))

  (def prom-path "/etc/prometheus/prometheus.yml")

  (def prom-config
    (cmd/from-yaml
     (ssh/$> ztx fi `[cat ~prom-path])))


  (defn node-url [x]
    (str x ":9100"))


  (def scrapes
    {:aidbox
     {:scrape_interval "5s",
      :metrics_path "/metrics",
      :static_configs [{:targets [(-> insts :aidbox-perf-aidbox :ip (str \: 8765))]}]}
     :aidbox-minutes
     {:scrape_interval "30s",
      :metrics_path "/metrics/minutes",
      :static_configs [{:targets [(-> insts :aidbox-perf-aidbox :ip (str \: 8765))]}]}
     :aidbox-hours
     {:scrape_interval "1m",
      :metrics_path "/metrics/hours",
      :static_configs [{:targets [(-> insts :aidbox-perf-aidbox :ip (str \: 8765))]}]}
     :perf-aws-c6i-xl-test
     {:scrape_interval "5s",
      :static_configs [{:targets [(node-url (:ip (:aidbox-perf-test insts)))]}]}
     :perf-aws-c6i-xl-db
     {:scrape_interval "5s",
      :static_configs [{:targets [(node-url (:ip (:aidbox-perf-db insts)))]}]}
     :perf-aws-c6i-xl-aidbox
     {:scrape_interval "5s",
      :static_configs [{:targets [(node-url (:ip (:aidbox-perf-aidbox insts)))]}]}})


  (def new-prom-cfg
    (add-scrapes prom-config scrapes))

  (def prom-yaml (cmd/tmp-file (cmd/to-yaml new-prom-cfg)))

  (ssh/scp-to ztx fi prom-yaml prom-path)

  (cmd/lns
   (ssh/$> ztx fi '[systemctl restart prometheus]))

  (cmd/lns
   (ssh/$> ztx fi '[systemctl status prometheus]))

  (cmd/lns
   (ssh/$> ztx fi `[cat ~prom-path]))


  (cmd/lns
   (ssh/$> ztx fi `[sy]))



  ;; graf fi.hz.health-samurai.io:3001
  ;; prom fi.hz.health-samurai.io:9099
  ;; push fi.hz.health-samurai.io:9099



  ;; test -> node exporter
  ;; test -> pushgateway


  ;; start instances
  ;; start db
  ;; start aidbox
  ;; start test




  )
