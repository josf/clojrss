(ns net.ramure.clojrss
  (:import (java.io.BufferedReader FileReader)))

(defstruct feed :name :title :url :type :etag :last-update)

(defn init-feed-db [] 
  #{})

(defn add-feeds [db feed] (into db feed))

;; from http://groups.google.com/group/clojure/browse_thread/thread/380fdd164a5c5b7a/6c2ce06780ce5ddf?lnk=gst&q=line-seq&pli=1

(defn file-lines
         [file-name]
         (line-seq (BufferedReader. (FileReader. file-name))))


(defn parse-rsslist-file [db feedlist-filename]
 (with-open [r (reader feedlist-filename)]
   (doseq [line (line-seq r)]
     (let [url (first (.split line " "))
           nam (second (.split line " "))]
       (println "adding")
       (add-feeds db (struct feed nam nil url nil))))))


(defn parse-rsslist-file [db urlfile]
  (with-open [r (reader urlfile)]
    (parssrss db (line-seq r))))


(defn parssrss [db sq]
  (if (not sq)
    db
    (parssrss (db-add-line db (first sq)) (rest sq))))

(defn db-add-line [db line]
  (let [lspl (.split line " ")]
    (cons (struct-map feed 
                  :name (second lspl)
                  :title nil
                  :url (first lspl)) db)))




(defn parse-single-rss-line [line]
  ()

(with-open [r (reader feed-file-name)]
        (doseq [line (line-seq r)]
          (println (first (rest (.split line " "))))))


           