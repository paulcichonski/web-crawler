(ns cichonski.crawler
    ^{:author "Paul Cichonski"
    :doc "crawls and stores html pages.

           work-in-progress"}
    (:use [clojure.string :only (split replace-first)])
    (:require [net.cgrand.enlive-html :as enlive]
              [clojurewerkz.urly.core :as urly])
    (:import [clojurewerkz.urly UrlLike]
             [java.io File]))

(comment "the follow-redirect logic works, but it still doesn't handle everything, clojure.org has a redirect scheme to wiki.sessions to track users,
and then back to clojure.org with a session token...it traveerses the links but still gets nothing back. TODO: fix")
(defn- fetch-page [url]
  (letfn [(get-response-code [conn] (.getHeaderField conn 0))
          (redirect? [response-code] (= (re-find #"302" response-code) "302"))
          (follow-redirects [url]
                            (loop [base-url (java.net.URL. url)
                                   base-conn (.openConnection base-url)
                                   base-response-code (get-response-code base-conn)]
                              (if (false? (redirect? base-response-code)) base-url
                                (let [new-url (java.net.URL. (.getHeaderField base-conn "location"))
                                      new-conn (.openConnection new-url)
                                      new-response (get-response-code new-conn)
                                      randoval (str "tt" new-response)]
                                  (recur new-url new-conn new-response)))))]
  (enlive/html-resource (follow-redirects url))))



(defn- local-link? [^String target ^String parent]
  "ensure that the link is in the same domain as the parent node (i.e., doesn't point to different domain)"
  (let [target (urly/url-like target)
        parent (urly/url-like parent)]
    (cond (false? (urly/absolute? target)) true
          (= (urly/host-of target) (urly/host-of target)) true
          (not= (urly/host-of target) (urly/host-of target)) nil
          :else (throw (Exception. (str "not able to detrmine if target link is local, need new case for: " target))))))

(defn- store-page [html-map file-loc]
  (let [file-loc (if (= (.substring file-loc 0 1) "/") ;;--> needed for mac, not sure about other OS.
                   (replace-first file-loc #"/" "") 
                   file-loc)]
    (if (false? (.exists (File. file-loc)))  ;; if no file exists you need to create it, first create dir structure.
      (let [dir-componets (pop (split file-loc #"/"))
            dir-structure (reduce (fn [x y] (str x "/" y)) dir-componets)]
        (if (> (count dir-componets) 0) 
          (.mkdirs (File. dir-structure)))
        (.createNewFile (File. file-loc))))
    (spit file-loc (apply str (enlive/emit* html-map)))))


(comment "would like to use a zipper here for in-line editing of the tree (i.e., to make all links local), but may not be possible since cannot 
rely on all html being valid xml.....actually it seems like enlive returns a valid xml map, so give it a shot later")

(defn crawl-domain 
  ([start] (crawl-domain start 3 "" nil))
  ([start depth directory pred]
    "start -> page to start crawling from
     depth -> how deep into the page-graph to crawl (each linked page is a node)
     directory -> where to store the files
     pred -> every html element in start will be run through pred, if false that node and all children will be ignored"
    (let [grab-domain-links (fn [all-links] 
                              (filter #(local-link? ((% :attrs) :href) start) all-links))
          start-urly (urly/url-like start)
          root-page (fetch-page start)
          domain-links (grab-domain-links (enlive/select root-page [:a]))]
      (store-page root-page (str directory "root.html"))
      
      ;; the slow way - using non-tail recursion....but works!
      (letfn [(wrong-way [links dp]
                         (for [link links] ;; depth-first, each link from the base will start its own recursion tree down a spine
                           (let [valid-link (.toURL (urly/resolve start-urly (urly/url-like link)))]
                             (if (> dp 1) 
                               (store-page (fetch-page link) (str (urly/path-of (urly/url-like valid-link)) ".html"))
                               (wrong-way (grab-domain-links (enlive/select link [:a]))
                                      (dec dp))))))]
             (wrong-way (map #((% :attrs) :href) domain-links) depth)))))
      
      ;; the "right" way, but not yet working --> cannot have the for() inside the loop, since that makes it so recur is not in tail...need to fix or this is going to be slow!
      (comment (loop [links (map #((% :attrs) :href) domain-links)
             dp depth]
        (for [link links] ;; depth-first, each link from the base will start its own recursion tree down a spine
          (let [valid-link (.toURL (urly/resolve start-urly (urly/url-like link)))]
            (if (> dp 1) 
              (store-page (fetch-page link) (str (urly/path-of (urly/url-like valid-link)) ".html"))
              (recur (grab-domain-links (enlive/select link [:a]))
                     (dec dp)))))))




(comment "next work period goals:
          1) make it so tail-recursion works on crawl-domain
          2) fix bugs, new ones crop up for each domain searched....
          3) write code for ensuring all links to children-pages are made local
          4) store images")