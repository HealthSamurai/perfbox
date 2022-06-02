(ns opsrepl.aws
  (:require
   [opsrepl.cmd :as cmd]
   [cheshire.core :as json]))


(defn map-inst [lst]
  (->> lst
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


(defn inst-list
  "list aws instances"
  [& [{_region :region :as opts}]]
  (->
   (cmd/$> `[aws ec2 describe-instances ~opts])
   (cmd/from-json)
   (map-inst)
   (->> 
    (reduce (fn [acc inst] (assoc acc (keyword (or (get-in inst [:tags "Name"]) (:id inst))) inst))
            {}))))

(defn by-name [xs nm]
  (->> xs
       (filter (fn [{tgs :tags}]
                 (= (get tgs "Name") (name nm))))
       (first)))

(defn reg-list
  "list regions"
  []
  (->>
   (cmd/$> '[aws ec2 :describe-regions])
   (cmd/from-json)
   :Regions
   (mapv (fn [x] (:RegionName x)))))

(defn with-regs [f]
  (->>
   (cmd/$> '[aws ec2 :describe-regions])
   (cmd/from-json)
   :Regions
   (mapcat (fn [x]
             (println :region (:RegionName x))
             (f (:RegionName x))))))

(defn inst-start [inst]
  (let [ids (:id inst)]
    (-> 
     (cmd/$> `[aws ec2 start-instances {:-instance-ids ~ids}])
     cmd/from-json)))

(defn inst-stop [inst]
  (let [ids (:id inst)]
    (-> 
     (cmd/$> `[aws ec2 stop-instances {:-instance-ids ~ids}])
     cmd/from-json)))

(defn inst-terminate [inst]
  (let [ids (:id inst)]
    (-> 
     (cmd/$> `[aws ec2 terminate-instances {:instance-ids ~ids}])
     cmd/from-json)))

;; TODO fix
(defn inst-create [nm & [opts]]
  (let [inst (merge 
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
                       (get-in opts [:volume :size] 51)
                       (get-in opts [:volume :type] "gp3"))
               :count  1}
              (dissoc opts :volume))]
    (cmd/$> `[aws ec2 run-instances ~inst])))


(defn vols-list [& [opts]]
  (->> 
   (cmd/$> `[aws ec2 describe-volumes ~opts])
   (cmd/from-json)
   :Volumes
   (mapv (fn [x]
           {:id (:VolumeId x)
            :inst (get-in x [:Attachments 0 :InstanceId])
            :status (:State x)
            :tags (->> (:Tags x)
                       (reduce (fn [acc {k :Key v :Value}]
                                 (assoc acc k v)) {}))
            :size (:Size x)}))))

(defn ssh-string [inst]
  (str "TERM=xterm-256color ssh -i perf-tests-instance.pem ubuntu@" (:ip inst)))



(comment

  (with-regs
    (fn [reg] (inst-list {:-region reg})))
  
  (reg-list)

  (def insts (inst-list))

  (:ip (:aidbox-perf-test insts))
  ;; => "3.236.168.168"


  (inst-start (:aidbox-perf-test insts))

  (ssh-string (:aidbox-perf-test insts))

  (vols-list)


  )
;; => nil
