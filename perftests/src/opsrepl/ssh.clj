(ns opsrepl.ssh
  (:require
   [opsrepl.cmd :as cmd]
   [cheshire.core :as json]
   [clj-yaml.core]
   [clojure.string :as str]
   [clj-ssh.ssh :as s])
  (:import [java.io File]))

;; todo .ssh/id_rsa may be default
;; TODO: stop if exists
(defn ssh-agent [ztx agent-name
                 {:keys [^String name
                         ^String public-key
                         ^String private-key
                         ^String public-key-path
                         ^String private-key-path
                         ^Identity identity
                         ^bytes passphrase]
                  :as opts}]
  (let [ag (s/ssh-agent {:use-system-ssh-agent false})]
    (s/add-identity ag opts)
    (swap! ztx assoc-in [:ssh/agents agent-name] {:agent ag})
    :ok))

(defn session [ztx agent-name session-name {ip :ip user :user }]
  (if-let [ag (get-in @ztx [:ssh/agents agent-name :agent])]
    (let [ses (s/session ag ip {:username (or user "root")
                                :strict-host-key-checking :no})]
      (swap! ztx assoc-in [:ssh/agents agent-name :sessions session-name] {:session ses
                                                                           :connection {:ip ip
                                                                                        :username (or user "root")
                                                                                        :strict-host-key-checking :no}})
      :ok)
    :error/no-agent))


(defn ->env [m]
  (->>
   m
   (mapv (fn [[k v]] (str (name k) "='" v "'")))
   (str/join " ")))

(defn get-session [ztx [agent-name session-name]]
  (get-in @ztx [:ssh/agents agent-name :sessions session-name :session]))

;; think about reconnect
(defn $> [ztx path cmd & [{:keys [env source wdir] :as opts}]]
  (let [cmd (if-let [e (:env opts)]
              (into ['export (->env e) '&&] cmd)
              cmd)
        cmd (if-let [src (:source opts)]
              (into ['source src '&&] cmd)
              cmd)
        cmd (if-let [wdir (:wdir opts)]
              (into ['cd wdir '&&] cmd)
              cmd)
        cmd-str (cmd/u cmd)
        _ (prn cmd-str)]
    (if-let [ses (get-session ztx path)]
      (let [res (s/ssh ses {:cmd cmd-str})]
        (cmd/proc-res (assoc res :cmd cmd-str)))
      :error/no-session)))

(defn scp-to [ztx path src dest]
  (s/scp-to (get-session ztx path) src dest))

(defn scp-from [ztx path src dest]
  (s/scp-from (get-session ztx path) src dest))

(defn copy [ztx from-path to-path path & [dest-path]]
  (let [t (cmd/tmp-file)
        from (get-session ztx from-path)
        to (get-session ztx to-path)]
    (s/scp-from from path t)
    (s/scp-to to t (or dest-path path))))

(comment
  (def ztx (atom {}))

  (ssh-agent ztx :ec2 "perf-tests-instance.pem")

  (session ztx :ec2 :test {:ip "3.236.168.168" :user "ubuntu"})

  (cmd/lns
   ($> ztx [:ec2 :test] '[ps aux]))

  (scp-to ztx [:ec2 :test] "/tmp/q" "/tmp/q")

  ($> ztx [:ec2 :test] '[cat "/tmp/q"])

  ztx

  )
