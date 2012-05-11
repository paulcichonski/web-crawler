(ns cichonski.crawler
    ^{:author "Paul Cichonski"
    :doc "crawls and stores html pages.

           work-in-progress"}
    (:use [clojure.string :only (split)])
    (:require [net.cgrand.enlive-html :as enlive]))

(defn- fetch-url [url]
  (enlive/html-resource (java.net.URL. url)))

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

(comment "there are a ton of ways for a link to be local...will need to build this up")
(defn- local-link? [target domain]
  "ensure that the link is to something in this domain"
  (cond (= (.charAt target 0) \/) true
        (= (parse-domain target) domain) true
        (not= (parse-domain target) domain) nil
        :else (throw (Exception. (str "not able to detrmine if target link is local, need new case for: " target)))))



(comment "would like to use a zipper here for in-line editing of the tree (i.e., to make all links local), but may not be possible since cannot 
rely on all html being valid xml.....actually it seems like enlive returns a valid xml map, so give it a shot later")

(defn crawl-domain 
  ([start] (crawl-domain start nil "." nil))
  ([start depth directory-to-store pred]
    "start -> page to start crawling from
     depth -> how deep into the page-graph to crawl (each linked page is a node)
     directory-to-store -> where to store the files
     pred -> every html element in start will be run through pred, if false that node and all children will be ignored"
    (let [domain (parse-domain start)
          root-page (fetch-url start)
          links (enlive/select root-page [:a])
          domain-links (filter #(local-link? ((% :attrs) :href) domain) links)]
      domain-links)))