;(ns net.ramure.clojrss
;  (:import (java.io.BufferedReader FileReader)))

(import '(java.net URL)
        '(java.lang StringBuilder)
        '(java.io BufferedReader InputStreamReader))

(import '(clj-http-client.core))
(use '[clojure.contrib.duck-streams :only (reader)])
(defstruct feed :name :title :url :type :etag :last-update)

(defn init-feed-db [] 
  #{})

;; from http://groups.google.com/group/clojure/browse_thread/thread/380fdd164a5c5b7a/6c2ce06780ce5ddf?lnk=gst&q=line-seq&pli=1

(defn parse-rsslist-file [db urlfile]
  (with-open [r (reader urlfile)]
    (parssrss  (line-seq r) db)))


(defn parssrss [sq db]
  (if (not sq)
    db
    (recur (rest sq) (db-add-line db (first sq)))))

(defn db-add-line [db line]
  (let [lspl (.split line " ")]
    (cons (struct-map feed 
                  :name (second lspl)
                  :title nil
                  :url (first lspl)) db)))

(defn fetch-url
  "Return the web page as a string."
  [address]
  (let [url (URL. address)]
    (with-open [stream (. url (openStream))]
      (let [buf (BufferedReader. (InputStreamReader. stream))]
        (apply str (line-seq buf))))))


;;; getting ETag and Last-Modified
(let [[status headers body] (http-get "http://ajaxian.com/index.xml")]
        (list (get headers "ETag") (get headers "Last-Modified"))) 

(defn check-feed
  "Check if feed has been updated and grab it if it has."
  [feed etag lmodif]
  (let [[status headers body] (http-get feed { "If-Modified-Since" lmodif 
                                               "If-None-Matc" etag })]
    (println (str "Status: " status))
    (doseq [h (keys headers)]
      (println (str h " " (get headers h))))
    (when (or (= status 200)
              (= status 304))
      (list (get headers "ETag") (get headers "Last-Modified")))))
      