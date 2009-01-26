;(ns net.ramure.clojrss
;  (:import (java.io.BufferedReader FileReader)))

(import '(java.net URL)
        '(java.lang StringBuilder)
        '(java.io BufferedReader InputStreamReader))

(use '[clj-http-client.core])
(use '[clojure.contrib.duck-streams :only (reader spit)])
(defstruct feed :name :title :url :type :etag :lmodif)

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

(defn db-save [db filename]
  (spit 
   filename 
   (with-out-str (print db))))

(defn db-load [filename]
  (with-in-str  (slurp filename)
    (read)))


;;; getting ETag and Last-Modified
(let [[status headers body] (http-get "http://ajaxian.com/index.xml")]
        (list (get headers "ETag") (get headers "Last-Modified"))) 

(defn check-feed
  "Check if feed has been updated and grab it if it has. Returns a
vector [body status ETag Last-Modified"

  [feed etag lmodif]
  (let [[status headers body] (http-get feed 
                                        (cond
                                         (and lmodif etag)
                                         { "If-Modified-Since" lmodif 
                                           "If-None-Match" etag}
                                         etag
                                         {"If-None-Match" etag}
                                         lmodif  
                                         { "If-Modified-Since" lmodif }))]
    (when (and status
               (or (= status 200)
                   (= status 304)))
      (println (str "Status: " status))
      [ body status  
        (get headers "ETag") 
        (get headers "Last-Modified")])))
      
(defn feed-struct-update [feed]
  (let [feedvec (check-feed 
                 (str (get feed :url))
                 (str (get feed :lmodif))
                 (str (get feed :etag)))]
    (when (= (second feedvec) 200) ; status
      (write-rss-file (get feed :name) (first feedvec))
      (assoc feed  
        :etag (nth feedvec 2)
        :lmodif (nth feedvec 3)))))

(defn write-rss-file [filename data]
  (spit (rss-filename filename) data))

(defn rss-filename [filename]
  (let [f (str filename)]
   (if
       (= (.indexOf f ".xml") -1)
     (str f ".xml")
     f)))

