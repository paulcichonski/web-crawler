(ns cichonski.crawler
    ^{:author "Paul Cichonski"
    :doc "crawls and stores html pages.

           work-in-progress"}
    (:use [clojure.string :only (split)])
    (:require [net.cgrand.enlive-html :as enlive]
              [clojurewerkz.urly.core :as urly])
    (:import [clojurewerkz.urly UrlLike]))

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
  (spit file-loc (apply str (enlive/emit* html-map))))


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
      (loop [link (((first domain-links) :attrs) :href) ;; HACK to test, need to go throuh entire list!
             dp depth]
        (let [valid-link (.toURL (urly/resolve start-urly (urly/url-like link)))]
          (if (> dp 1) 
                (store-page (fetch-page link) "root/link1.html")
                (recur (first (grab-domain-links (enlive/select link [:a]))) ;; HACK to test, need to go throuh entire list!
                       (dec dp))))))))




(comment "todays goals:
          1) write code for storing pages
          2) write code for recursing through link structure to the requested depth
          3) write code for ensuring all links to children-pages are made local
          4) store images")