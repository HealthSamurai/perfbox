(ns opsrepl.chrome
  (:require [clojure.java.io :as io]
            [gniazdo.core :as ws]
            [cheshire.core]
            [clojure.java.shell]
            [org.httpkit.client]))

;; methods https://chromedevtools.github.io/devtools-protocol/tot/Page/

(defonce *id (atom 1))

(defn- decode-base64 [^String s]
  (.decode (java.util.Base64/getDecoder) s))

(defn export-to-file [cmd-output filename]
  (-> cmd-output :data decode-base64
      (java.io.ByteArrayInputStream.)
      (io/copy (io/file filename))))

(defmulti process (fn [ztx msg] (:method msg)))
(defmethod process :default [ztx msg] (println :nop msg))

(defn dispatch [ztx {id :id :as msg}]
  (if-let [req (get-in @ztx [:ws/requests id])]
    (do 
      (println :< id (keys msg))
      (deliver req msg))
    (if (:method msg)
      (do 
        (println :<< (:method msg))
        (process ztx msg))
      (println :missed-message (:id msg)))))


(defn on-connect [ztx nm p]
  (println :on-connect nm)
  (deliver p :ok))

(defn on-error [ztx nm p err]
  (println :on-error nm (.getMessage err))
  (deliver p :error))

(defn on-close [ztx nm p code & [err]]
  (println :on-close nm)
  (deliver p :close))

(defn connect [ztx nm {url :webSocketDebuggerUrl :as page}]
  (let [p (promise)
        c (ws/connect url
            :on-receive (fn [x] (dispatch ztx (cheshire.core/parse-string x keyword)))
            :on-connect (fn [& _] (on-connect ztx nm p))
            :on-error (fn [err] (on-error ztx nm p err))
            :on-close (fn [code & [err]] (on-close ztx nm p code err)))]
    ;; todo reconnect
    (swap! ztx update-in [:ws/conns nm]
               (fn [old]
                 (when-let [c (:conn old)] (ws/close c))
                 {:conn c :page page})))
  :ok)

(defn req [url]
  (let [res @(org.httpkit.client/request {:url url})]
    (cheshire.core/parse-string (:body res) keyword)))

(defn init [ztx url] (swap! ztx assoc :chrome url))

(defn root-page [ztx]
  (let [chr (get @ztx :chrome "http://127.0.0.1:9222")]
    (req (str chr "/json/version"))))

(defn pages [ztx]
  (let [chr (get @ztx :chrome "http://127.0.0.1:9222")]
    (req (str chr "/json/list"))))

(defn rpc [ztx conn-name meth & [params]]
  (swap! ztx update :ws/id (fn [x] (inc (or x 0))))
  (if-let [sock (get-in @ztx [:ws/conns conn-name :conn])]
    (let [id (get @ztx :ws/id) 
          promise (promise)
          msg (cheshire.core/generate-string (cond-> {:id id :method (name meth)}
                                               params (assoc :params params)))]
      (println :> id meth)
      (ws/send-msg sock msg)
      (swap! ztx assoc-in [:ws/requests id] promise)
      promise)
    :error))


(defn decode-base64 [^String s]
  (.decode (java.util.Base64/getDecoder) s))

(defn export-to-file [cmd-output filename]
  (-> cmd-output :data decode-base64
      (java.io.ByteArrayInputStream.)
      (io/copy (io/file filename))))

(defn show [ztx page-name]
  (let [res  @(rpc ztx page-name 'Page.captureScreenshot {:format "webp"})]
    (when-let [r (:result res)]
      (export-to-file r "/tmp/res.webp")
      (clojure.java.shell/sh "open" "/tmp/res.webp"))))

(comment
  (def ztx (atom {}))

  (init ztx "http://localhost:9922")

  (def root (root-page ztx))

  (pages ztx)

  (connect ztx :root root)
  (connect ztx :p1 (first (pages ztx)))

  (def c (get-in @ztx [:ws/conns :root :conn]))

  @(rpc ztx :root 'Target.createTarget {:url "https://grafana.aidbox.io/d/home/home?orgId=1&refresh=1m"})

  @(rpc ztx :p1 'Page.navigate {:url  "https://aidbox.app"})
  @(rpc ztx :p1 'Page.navigate {:url  "https://grafana.aidbox.io/d/home/home?orgId=1&refresh=1m"})

  (show ztx :p1)


  @(rpc ztx :p1 'Page.stopLoading)
  @(rpc ztx :p1 'DOM.getDocument)
  @(rpc ztx :p1 'DOM.getAttributes {:nodeId 4})
  
  @(rpc ztx :p1 'DOM.querySelectorAll {:nodeId 1 :selector "div"})
  @(rpc ztx :p1 'DOM.setAttributeValues {:nodeId 42 :name "value" :value "admin"})
  
  @(rpc ztx :p1 'Network.enable)
  @(rpc ztx :p1 'Network.disable)
  @(rpc ztx :p1 'Network.getAllCookies)

  @(rpc ztx :p1 'Page.reload)
  
  (show ztx :p1)

  @(rpc ztx :p1 'Page.navigate {:url  "https://grafana.aidbox.io/d/home/home?orgId=1&refresh=1m"})

  (def root-node
    (-> @(rpc ztx :p1 'DOM.getDocument)
        (get-in [:result :root :nodeId])))

  (def inp 
    (-> 
     @(rpc ztx :p1 'DOM.querySelectorAll {:nodeId root-node :selector "input[name=user]"})
     (get-in [:result :nodeIds 0])))

  @(rpc ztx :p1 'DOM.setAttributeValue {:nodeId inp :name "value" :value "admin"})

  (def pass
    (-> 
     @(rpc ztx :p1 'DOM.querySelectorAll {:nodeId root-node :selector "input[name=password]"})
     (get-in [:result :nodeIds 0])))

  @(rpc ztx :p1 'DOM.setAttributeValue {:nodeId pass :name "value" :value "u134zp2n"})

  @(rpc ztx :p1 'Runtime.evaluate {:expression "document.querySelector('button').click()"})

  (show ztx :p1)

  @(rpc ztx :p1 'Network.getAllCookies)

  @(rpc ztx :p1 'Network.enable)
  @(rpc ztx :p1 'Network.disable)
  


  )


