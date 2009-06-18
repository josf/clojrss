;(ns net.ramure.clojrss
;  (:import (java.io.BufferedReader FileReader)))

(ns net.ramure.clojrss
  (:use [clojure.contrib.duck-streams :only (reader spit)])
  (:use [clojure.contrib.zip-filter.xml])
  (:import [clojure.zip])
  (:use [saxon])
  (:use [clj-http-client.core]))

(import '(java.net URL)
        '(java.lang StringBuilder)
        '(java.io BufferedReader InputStreamReader))

(defstruct feed :name :title :url :type :etag :lmodif :xml)

(defn init-feed-db [] 
  #{})

;; from http://groups.google.com/group/clojure/browse_thread/thread/380fdd164a5c5b7a/6c2ce06780ce5ddf?lnk=gst&q=line-seq&pli=1

(defn db-add-line [db line]
  (let [lspl (.split line " ")]
    (cons (struct-map feed 
                  :name (pr-str (second lspl))
                  :title nil
                  :url (pr-str (first lspl)))
          db)))

(defn parssrss [sq db]
  (if (not sq)
    db
    (recur (rest sq) (db-add-line db (first sq)))))

(defn parse-rsslist-file [db urlfile]
  (with-open [r (reader urlfile)]
    (parssrss  (line-seq r) db)))

;; irc example
;;(binding [*print-dup* true] (println
;;          (sorted-set 1 2 3) "String"))
(defn db-save [db filename]
  (spit 
   filename 
   (with-out-str (print db))))

(defn db-serializable [db]
  (into (empty db)
        (map 
         (fn [fd]
              (assoc fd 
                  (when (string? (get fd key))
                    (pr-str (get fd key))))
         db))))


(defn db-load [filename]
  (with-in-str  (slurp filename)
    (read)))

(defn check-feed
  "Check if feed has been updated and grab it if it has. Returns a
vector [body status ETag Last-Modified]"

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
      [body status  
       (str (get headers "ETag"))
       (str (get headers "Last-Modified"))])))

(defn rss-filename [filename]
  (let [dir "/home/joseph/localrss/dl/"
        f (str filename)]
   (if 
       (= (.indexOf f ".xml") -1)
     (str dir f ".xml")
     (str dir f))))

;;; copied from examples in zip-filter/xml.clj
(defn parse-str [s]
  (clojure.zip/xml-zip (clojure.xml/parse (new org.xml.sax.InputSource
                               (new java.io.StringReader s)))))

(defn write-rss-file [filename data]
  (spit (rss-filename filename) data))

(defn check-feed-type [xml]
"returns :atom if atomfeed, :rss otherwise"
  (let [xmlzip (parse-str xml)]
    (if (xml-> xmlzip  
               (attr= :xmlns "http://www.w3.org/2005/Atom") 
               (attr :xmlns))
      :atom
      :rss)))

(defn feed-struct-update [feed]
  (let [feedvec (check-feed 
                 (str (get feed :url))
                 (str (get feed :lmodif))
                 (str (get feed :etag)))
        feedtype (check-feed-type (first feedvec))]
    (when (= (second feedvec) 200) ; status. On a 304, we do not write anything.
      (write-rss-file 
       (get feed :name) 
       (if (= feedtype :atom)
         (atom-to-rss (first feedvec))
         (first feedvec)))
      (assoc feed  
        :etag  (nth feedvec 2)
        :lmodif (nth feedvec 3)
        :type feedtype))))

(defn atom-to-rss [atomf]
   ((compile-xslt (compile-file "/home/joseph/clojrss/atom2rss.xsl")) 
    (compile-string atomf)))


(defn replace-feed-by-url [db url nfeed]
  "Finds a feed in the db and replaces it with nfeed. Returns the new
db."
  (into (empty db)
        (map 
         (fn [feed]
           (if (= (str (get feed :url))
                  url)
             nfeed
             feed))
         db)))
             
(defn update-feed [db feed]
  "Checks feed and returns an updated version of db."
  (replace-feed-by-url db
                       (get feed :url)
                       ;updating file is a side effect here
                       (feed-struct-update feed))) 

(defn check-all-feeds [db]
  (into (empty db)
        (map 
         (fn [feed]
           (feed-struct-update feed))
        db)))
             
    