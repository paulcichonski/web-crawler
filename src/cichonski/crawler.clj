(ns cichonski.crawler
    ^{:author "Paul Cichonski"
    :doc "crawls and stores html pages.

           work-in-progress"}
    (:use [clojure.string :only (split)])
    (:require [net.cgrand.enlive-html :as enlive]))

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

(defn- parse-domain [url]
  "parse the domain. TODO: this is currently really naive, improve!"
  ;; use "www.google.com as an example, want to return the google.com part, note: this could also be video.google.com
  ;; break out the "x.google.x" part, which is returend from the regex and concat second two elements
  (try 
    (let [domain-components (split (second (re-find #"^(?:\w*://)?([^/?#]+)?(.*?)" url)) #"\." )]
      (cond (= (count domain-components) 3) (str (second domain-components) "." (nth domain-components 2))
            (= (count domain-components) 2) (str (first domain-components) "." (second domain-components))))
    ;; TODO: try/catch is here for debugging, need to remove.
    (catch Exception e (throw (Exception. (str "could not find domain of URL: " url (.getMessage e)))))))

(comment "TODO: use this: https://github.com/michaelklishin/urly for doing this logic")
(defn- local-link? [target domain]
  "ensure that the link is to something in this domain"
  (cond (= (.charAt target 0) \/) true
        (= (parse-domain target) domain) true
        (not= (parse-domain target) domain) nil
        :else (throw (Exception. (str "not able to detrmine if target link is local, need new case for: " target)))))

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
    (let [domain (parse-domain start)
          root-page (fetch-page start)
          links (enlive/select root-page [:a])
          domain-links (filter #(local-link? ((% :attrs) :href) domain) links)]
      (store-page root-page (str directory "root.html"))
      (loop [link (((first domain-links) :attrs) :href)
             dp depth]
        (if (> dp 1) 
          (store-page (fetch-page link) "root/link1.html")
          (recur (first (filter #(local-link? ((% :attrs) :href) domain) (enlive/select link [:a])))
                 (dec dp)))))))




