(ns opsrepl
  (:require
   [clojure.java.shell :refer [sh] :as shell]
   [cheshire.core :as json]
   [clj-yaml.core]
   [clojure.string :as str]
   [clj-ssh.ssh :as s])
  (:import [java.io File]))


(defn mk-opts [opts]
  (when opts
    (str " "
         (->> opts
              (mapv (fn [[k v]]
                      (str "--"(name k) " "
                           (cond
                             (map? v)
                             (->> (mapv (fn [[k v]]
                                          (str "key=" (name k)",value=" v)) v)
                                  (str/join " "))
                             (sequential? v)
                             (str/join " " v)
                             :else v))))
              (str/join " ")))))

(defn ls [cmd & [opts]]
  (let [cmd (str "aws lightsail --output json " (name cmd) (mk-opts opts))
        _ (println :>> cmd)
        res (sh "bash" "-c" cmd)]
    (println :?? res)
    (if (= (:exit res) 0)
      (json/parse-string (:out res)
       keyword)
      (do 
        (println :Error)
        (println (:err res))
        :error))))

(defn aws [cmd & [opts]]
  (let [cmd (str "aws ec2  --output json " (name cmd) (mk-opts opts))
        _ (println :>> cmd)
        res (sh "bash" "-c" cmd)]
    (if (= (:exit res) 0)
      (json/parse-string
       (:out res)
       keyword)
      (do 
        (println :Error)
        (println (:err res))
        :error))))

(defn inst-list [& [opts]]
  (->> 
   (aws :describe-instances opts)
   :Reservations
   concat
   (mapcat :Instances)
   (mapv (fn [x]
           {:id (:InstanceId x)
            ;; :orig x
            :tags (->> (:Tags x)
                       (reduce (fn [acc {k :Key v :Value}]
                                 (assoc acc k v)) {}))
            :type (:InstanceType x)
            :status (get-in x [:State :Name])
            :ip (:PublicIpAddress x)
            :private-ip (get-in x [:NetworkInterfaces 0 :PrivateIpAddress])}))))

(defn inst-list-all []
  (->>
   (aws :describe-regions)
   :Regions
   (mapv :RegionName)
   (mapcat (fn [region] (inst-list {:region region})))))


(defn inst-start [insts]
  (aws :start-instances {:instance-ids (if (string? (first insts)) insts (mapv :id insts))}))

(defn inst-stop [insts]
  (aws :stop-instances {:instance-ids (if (string? (first insts)) insts (mapv :id insts))}))

(defn inst-terminate [insts]
  (aws :terminate-instances {:instance-ids (if (string? (first insts)) insts (mapv :id insts))}))

:iops
;; gp3 : 3,000-16,000 IOPS
;; io1 : 100-64,000 IOPS
;; io2 : 100-64,000 IOPS

;; "DeviceName=string,VirtualName=%s,Ebs={DeleteOnTermination=true,Iops=%s,VolumeSize=%s,VolumeType=%s,Throughput=integer,OutpostArn=string,Encrypted=false}"
(defn inst-create [nm & [opts]]
  (aws :run-instances
      (merge 
       {:image-id " ami-005cd049e6178d921"
        :instance-type "c6i.xlarge"
        :key-name "perf-tests-instance"
        :security-group-ids "sg-0b54bc52a8ee0279f" 
        :subnet-id "subnet-08486b7cdb8742cd8"
        :tag-specifications (format "'ResourceType=instance,Tags=[{Key=Name,Value=%s}]' 'ResourceType=volume,Tags=[{Key=Name,Value=%s}]'" nm nm)
        :placement "'AvailabilityZone=us-east-1d'"
        :block-device-mappings
        (format "'DeviceName=%s,VirtualName=%s,Ebs={DeleteOnTermination=true,VolumeSize=%s,VolumeType=%s}'"
                "/dev/sda1"
                nm
                ;; (get-in opts [:volume :iops] 150)
                (get-in opts [:volume :size] 51)
                (get-in opts [:volume :type] "gp3"))
        :count  1}
       (dissoc opts :volume))))


(defn vols-list [& [opts]]
  (->> 
   (aws :describe-volumes opts)
   :Volumes
   (mapv (fn [x]
           {:id (:VolumeId x)
            :inst (get-in x [:Attachments 0 :InstanceId])
            :status (:State x)
            :tags (->> (:Tags x)
                       (reduce (fn [acc {k :Key v :Value}]
                                 (assoc acc k v)) {}))
            :size (:Size x)}))))

(defn vols-list-all []
  (->>
   (aws :describe-regions)
   :Regions
   (mapv :RegionName)
   (mapcat (fn [region] (vols-list {:region region})))))

(defn run []
  (->> (inst-list)
       (mapv println))

  (->> (vols-list)
       (mapv println)))


(defn connect []
  (->> (inst-list)
       (filter (fn [x] (= "running" (:status x))))
       (mapv (fn [i]
               (println :connect-to (:tags i) (:status i))
               (println (str "TERM=xterm-256color ssh -i perf-tests-instance.pem ubuntu@" (:ip i)))))))

(defn ssh [inst cmd]
  (when inst
    (let [res (sh "bash" "-c" (str "ssh -i perf-tests-instance.pem ubuntu@" (:ip inst)" '" cmd "'"))]
      (if (= 0 (:exit res))
        (:out res)
        (do
          (println (:err res)) :error)))))

(defn tmp-file [& [content]]
  (let [f (File/createTempFile "tmp" "ssh")]
    (when content
      (spit (.getPath f) content))
    (.getPath f)))

(defn scp [inst target source]
  (when inst
    (let [res (sh "bash" "-c" (str "scp -i perf-tests-instance.pem " source " ubuntu@" (:ip inst)":" target))]
      (if (= 0 (:exit res))
        (:out res)
        (do
          (println (:err res)) :error)))))


(defn by-name [xs nm]
  (->> xs
       (filter (fn [{tgs :tags}]
                 (= (get tgs "Name") (name nm))))
       (first)))

(defn prometheus [insts]
  (let [{dip :private-ip} (by-name insts :aidbox-perf-db)
        {aip :private-ip} (by-name insts :aidbox-perf-aidbox)]
    {:global
     {:scrape_interval "15s",
      :evaluation_interval "15s",
      :external_labels {:monitor "aidbox"}},
     :scrape_configs
     [{:job_name "prometheus",
       :scrape_interval "5s",
       :static_configs [{:targets ["localhost:9090"]}]}
      {:job_name "pushgateway",
       :scrape_interval "5s",
       :honor_labels true,
       :static_configs [{:targets ["pushgateway:9091"]}]}
      {:job_name "aidbox",
       :scrape_interval "5s",
       :metrics_path "/metrics",
       :static_configs [{:targets [(str aip ":" 8766)]}]}
      {:job_name "aidbox-minutes",
       :scrape_interval "30s",
       :metrics_path "/metrics/minutes",
       :static_configs [{:targets [(str aip ":" 8766)]}]}
      {:job_name "aidbox-hours",
       :scrape_interval "1m",
       :metrics_path "/metrics/hours",
       :static_configs [{:targets [(str aip ":" 8766)]}]}
      ;; {:job_name "multibox",
      ;;  :scrape_interval "5s",
      ;;  :metrics_path "/metrics",
      ;;  :static_configs [{:targets ["host.docker.internal:8789"]}]}
      ;; {:job_name "multibox-minutes",
      ;;  :scrape_interval "30s",
      ;;  :metrics_path "/metrics/minutes",
      ;;  :static_configs [{:targets ["host.docker.internal:8789"]}]}
      ;; {:job_name "multibox-hours",
      ;;  :scrape_interval "1m",
      ;;  :metrics_path "/metrics/hours",
      ;;  :static_configs [{:targets ["host.docker.internal:8789"]}]}
      {:job_name "test-node",
       :scrape_interval "5s",
       :static_configs [{:targets ["host.docker.internal:9100"]}]}
      {:job_name "db-node",
       :scrape_interval "5s",
       :static_configs [{:targets [(str dip ":" 9100)]}]}
      {:job_name "ab-node",
       :scrape_interval "5s",
       :static_configs [{:targets [(str aip ":" 9100)]}]}]}))


(defn to-yaml [x]
  (clj-yaml.core/generate-string x))

(defn from-yaml [x]
  (clj-yaml.core/parse-string x))


(defn get-ag []
  (def ag (s/ssh-agent {:use-system-ssh-agent false}))
  (s/add-identity ag {:private-key-path "perf-tests-instance.pem"})
  ag)

(defn get-ses [srv]
  (if srv 
    (s/session ag (:ip srv) {:username "ubuntu" :strict-host-key-checking :no})
    :error/no-ip))


(defn u [& args]
  (->> args
       (reduce (fn [acc x]
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
                         :else (str x))))
               [])
       (str/join " ")))

(defn env [m]
  (->> 
   m
   (mapv (fn [[k v]] (str (name k) "='" v "'")))
   (str/join " ")))

(defn cmd [ses cmd & [opts]]
  (let [cmd (if-let [e (:env opts)]
              (into ['export (env e) '&&] cmd)
              cmd)
        cmd (if-let [src (:source opts)]
              (into ['source src '&&] cmd)
              cmd)
        cmd (if-let [wdir (:wdir opts)]
              (into ['cd wdir '&&] cmd)
              cmd)
        cmd-str (u cmd)
        res (s/ssh ses {:cmd cmd-str})]
    (println :cmd cmd-str)
    (if (= 0 (:exit res))
      (:out res)
      (do 
        (if (map? res)
          (do (println :exit (:exit res))
              (println :error (:err res))
              (println :out (:out res)))
          (println :error (type res)))
        (throw (Exception. "error"))))))

(defn lines [s]
  (str/split s #"\n"))

(defn cmdl [& args]
  (lines (apply cmd args)))

(defn split-cols [l]
  (str/split l #"\s+"))

(defn table [s]
  (let [lns (lines s) 
        cols (mapv keyword (split-cols (first lns)))]
    (->> (rest lns)
         (mapv (fn [x] (zipmap cols (split-cols x)))))))

(defn cmdt [& args]
  (table (apply cmd args)))

(defn cmdj [args]
  (cheshire.core/parse-string (apply cmd args) keyword))



(defn scat [ses dest]
  (cmd ses (str "cat " dest)))

(defn sls [ses dest]
  (lines (cmd ses (str "ls " dest))))

(defn sps [ses]
  (lines (cmd ses (str "ps aux"))))

(defn scurl [ses url]
  (cmd ses (str "curl " url)))

(defn scp-to [ses src dest]
  (s/scp-to ses src dest))

(defn scp-from [ses src dest]
  (s/scp-from ses src dest))

(defn copy [from to path & [dest-path]]
  (let [t (tmp-file)]
    (s/scp-from from path t)
    (s/scp-to to t (or dest-path path))))

(defn syaml [ses dest content]
  (scp-to ses (tmp-file (to-yaml content)) dest))

(defn browse [inst url port]
  (println :open (str "http://" (:ip inst) ":" port))
  (shell/sh "open" (str "http://" (:ip inst) ":" port))
  (str "http://" (:ip inst) ":" port))


(comment
  (inst-list)
  (inst-list-all)

  (inst-stop ["i-0e58207288bc8ad2e"])

  (inst-terminate
   (inst-list)
   )

  (println "ups")

  (vols-list)
  (vols-list-all)

  (inst-start
   [{:id "i-09d76615298386b5d",
     :tags {"Name" "aidbox-perf-db"},
     :type "t4g.xlarge",
     :status "stopped",
     :ip nil,
     :private-ip nil}
    {:id "i-05721342a5ba4d5c4",
     :tags {"Name" "aidbox-perf-aidbox"},
     :type "t4g.xlarge",
     :status "stopped",
     :ip nil,
     :private-ip nil}])

  (inst-stop ["i-03873664478492f8f" "i-09d76615298386b5d"])

  (ls :get-bundles)

  [(inst-create "aidbox-perf-test" {})
   (inst-create "aidbox-perf-db" {})
   (inst-create "aidbox-perf-aidbox" {})]

  
  (inst-start ["i-0eaa8d5dbcbc2b7e2"])
  
  (inst-stop (inst-list))

  (inst-list)

  (inst-start
   (inst-list))

  (connect)

  (do 
    (def insts (inst-list))
    (def ab (by-name insts :aidbox-perf-aidbox))
    (def tb (by-name insts :aidbox-perf-test))
    (def db (by-name insts :aidbox-perf-db))

    (def ag (get-ag))

    (def tbs (get-ses tb))
    (def abs (get-ses ab))
    (def dbs (get-ses db)))

  (do
    (def ab-wd "/home/ubuntu/sansara/box")
    (def sdkman "$HOME/.sdkman/bin/sdkman-init.sh")
    (def dbo {:wdir ab-wd
              :env {:GIT_SSH_COMMAND "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"}})

    (def tbo {:wdir "/home/ubuntu/sansara/infrabox"
              :source sdkman
              :env {:GIT_SSH_COMMAND "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"}})

    (def abo {:wdir ab-wd
              :source sdkman
              :env {:GIT_SSH_COMMAND "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"}}))

  
  (cmdl tbs '[ls -lah])

  (cmdt tbs '[docker ps])

  (scp-to tbs (tmp-file (str (java.util.Date.))) "/tmp/ups")

  (cmdl tbs '[df -h])

  (cmdt tbs '[df -h])

  (scp-to tbs (tmp-file (to-yaml {:name "hi"})) "/tmp/ups")

  (syaml tbs "/tmp/ups" {:name (str (java.util.Date.))})

  (def prom-cfg "/home/ubuntu/sansara/box/audit/prometheus/prometheus.yml")

  (sls tbs prom-cfg)

  (syaml tbs prom-cfg (prometheus insts))

  (from-yaml (scat tbs prom-cfg))

  (sps tbs)
  (lines (scurl tbs "http://localhost:9100/metrics"))

  (cmdl tbs :systemctl :list-units {:-type "service" :-state "running"})

  (cmdt tbs '[sudo lsof -i -P -n])

  (cmdl tbs '[cat "/proc/cpuinfo"])

  (cmdl tbs "docker ps")
  (cmdt tbs '[docker ps])

  (lines (cmd tbs "cd /home/ubuntu/sansara/box && docker-compose up -d prometheus grafana"))

  (cmdt tbs '[cd "/home/ubuntu/sansara/box" && docker-compose ps])

  (cmdt tbs '[cd "/home/ubuntu/sansara/box" && docker-compose ps])

  (cmdl tbs '[docker-compose restart prometheus] {:wdir "/home/ubuntu/sansara/box"})
  (cmdl tbs '[docker-compose ps] {:wdir "/home/ubuntu/sansara/box"})
  

  (cmdj tbs "docker ps --format='{{json .}}'")
  (defn docker [cmd ]
    (u '[docker cmd ])

    )

  (browse tb "/" 9090)
  (browse tb "/" 3001)
  (browse ab "/" 8765)

  (cmdl tbs '[sudo apt install curl])

  (cmdl tbs '[docker ps {:a true}])

  (cmdt tbs '[snap list])

  (cmdt tbs '[docker ps])

  (cmdt tbs '[ps aux])
  
  (cmdl tbs '[ls -lah] {:wdir "/home/ubuntu"})
  (cmdl tbs '[sudo snap install lxd])

  ;; start docker
  

  (do
    (:ip db)

    (cmdl abs '[clj -h])


    (cmdl abs '[curl -s "https://get.sdkman.io" | bash])
    (cmdl abs '[sdk -h])
    (cmdl abs '[cat sdkman])

    (cmdl abs '[sdk -h] abo)

    (cmdl abs '[pwd] abo)

    (cmdl abs '[ps aux | grep "clojure"])

    (cmdl abs '[cat .env] abo)
    (cmdl abs '[clj "-X:devbox"] )

    (cmdl abs '[sudo ls -lah "/var/log"])

    (cmdl abs `[setsid make repl < "/dev/null" > "/tmp/repl-logs" "2>&1"]
          (update abo :env merge {:PGHOST (:ip db)}))

    (cmdl abs '[tail "-1000" "/tmp/repl-logs"])

    (cmdl abs '[env] (assoc abo :env {:PGHOST (:ip db)}))

    (cmdl abs '[cat Makefile] abo)
    (cmdl abs '[git pull origin master] abo)

    (cmdl abs '[git remote show origin] abo)
    (cmdl abs '[ls -lah] abo)
    (cmdl abs '[sudo chown -R "ubuntu:ubuntu" "."] abo)

    (cmdl abs '[ls "/home/ubuntu/.ssh"] abo)
    (cmdl abs '[ls -lahf "/home/ubuntu/.ssh"] abo)
    (cmdl tbs '[ls -lahf "/home/ubuntu/.ssh"] abo)


    (println (cmd abs '[cat "/home/ubuntu/.ssh/id_rsa.pub"] abo))

    (cmdl abs '[chmod "u+rw" -R "/home/ubuntu/.ssh"] abo)
    (cmdl abs '[ssh-keygen -q -t rsa -N "''" -f "~/.ssh/id_rsa" "<<<y" > "/dev/null" "2>&1"])
    (cmdl abs '[ls -lah "/home/ubuntu/.ssh"] abo)

    (cmdl abs '[ssh-keyscan -t rsa github.com | tee github-key-temp | sudo ssh-keygen -lf - >> "/etc/ssh/ssh_known_hosts"])

    (cmdl abs '[bash -c "'kill 32699'"])


    (cmdl abs '[cat ".nrepl-port"] abo)
 
    (cmdl abs '[env])
    (cmdl abs '[kill 33165])
    (cmdl abs '[man kill])
    
    (browse ab "/" 36795)
    (def ab-port 8765)
    (browse ab "/" ab-port)

    (connect)

    (cmdl abs `[export ~(env {:MYENV 1}) && env])
    (cmdl abs '[curl "http://localhost:8765"])
    (cmdl abs '[sudo apt install curl])

    (str (:ip ab) ":" (cmd abs '[cat ".nrepl-port"] abo))
    

    (cmdl tbs '[sudo lsof -i -P -n])
    (shell/sh [(str "bash -c '" (u '[sudo lsof -i -P -n]) "'")])

    (cmdl abs '[ps aux | grep java])

    (cmdl abs '[kill 35893 36107 36320])


    (cmdl dbs '[ls -lah] dbo)
    (cmdl dbs '[docker ps] dbo)
    (cmdl dbs '[docker rm -f pushgateway] dbo)
    (cmdl dbs '[make up] dbo)

    (cmdl dbs '[docker logs db] dbo)
    (cmdl dbs '[docker exec db psql -c "'select 1'"] dbo)

    (println "hi")

    (browse ab "/" 8765)
    (def grafana 3001)

    (browse tb "/" grafana)

    (cmdl tbs '[git pull origin master] abo)

    (cmdl abs '[ls "/home/ubuntu/.ssh"] abo)
    (cmdl tbs '[ls "/home/ubuntu/.ssh"] abo)

    (copy abs tbs "/home/ubuntu/.ssh/id_rsa")
    (cmdl tbs '[chmod "600" "/home/ubuntu/.ssh/id_rsa"])

    (copy abs tbs "/home/ubuntu/.ssh/id_rsa.pub")

    (cmdl tbs `[setsid bb repl < "/dev/null" > "/tmp/perf-logs" "2>&1"] tbo)

    (cmdl tbs `[tail "/tmp/perf-logs"] tbo)

    (cmdl tbs `[bb tasks] tbo)
    (cmdl tbs `[ps aux | grep bb] tbo)

    (:ip tb)


    (:private-ip tb)

    )

  )

"44.203.233.245"
46311
