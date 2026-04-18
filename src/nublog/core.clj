(ns nublog.core
  (:gen-class)
  (:require [hiccup2.core :as h]
            [clojure.spec.alpha :as s]
            [java-time.api :as jt]
            [clojure.java.io :as io]
            [clojure.java.shell]))

(def site-title "australorp.dev")
(def elias-prescott "Elias Prescott")

(defn icon
  [n]
  (-> (str "icons/" n ".svg")
      slurp
      h/raw))

(defn breadcrumbs
  [links]
  (let [formatted-links (vec (map (fn [[href text]]
                                    [:li [:a {:href href} text]])
                                  links))]
    [:nav.breadcrumbs {:aria-label "Breadcrumbs"}
     `[:ol ~@(assoc-in formatted-links
                       [(- (count formatted-links) 1) 1 1 :aria-current]
                       "page")]]))

(defn post->url
  [p]
  (str "/posts/" (:id p)))

(defn post->link
  [p]
  [:a {:href (post->url p)}
   (icon (:icon p))
   (:title p)])

(defn tag->url
  [t]
  (str "/tags/" t))

(defn base-template
  [title body]
  [:html
   [:head
    [:link {:rel "stylesheet"
            :href "/static/missing.min.css"}]]
   [:body
    [:header
     [:h1 [:a {:href "/"} site-title]]
     [:nav.tool-bar
      [:a.flex-row.align-items:center.crowded {:href "https://github.com/EliasPrescott"}
       "GitHub" (icon 'code-xml)]
      [:a.flex-row.align-items:center.crowded {:href "https://www.linkedin.com/in/elias-prescott"}
       "LinkedIn" (icon 'contact)]
      ]]
    (vec (cons :main body))
    [:footer
     "Made with "
     [:a {:href "https://clojure.org"} "Clojure"]
     " and "
     [:a {:href "https://missing.style"} "missing.css"]
     "."]]])

(defn post-page-template
  [post]
  (base-template
    (:title post)
    (vec (concat [(breadcrumbs
                    [["/" "Home"]
                     [(post->url post) (str "Post: " (:title post))]])
                  [:h1 (:title post)]
                  [:sub "Written by " (:author post) " on " [:time (:created post)]]
                  [:br]
                  [:br]
                  [:div.flex-row.crowded
                    [:strong "Tags:"]
                    (for [tag (:tags post)]
                      [:chip 
                       [:a {:href (tag->url tag)}
                        tag]])]]
                 (:body post)))))

(defn tags-page-template
  [tag post-list]
  (base-template
    tag
    [(breadcrumbs
       [["/" "Home"]
        [(tag->url tag) (str "Tag: " tag)]])
     [:h1 tag " posts (" (count post-list) ")"]
     `[:ul ~@(map (fn [post]
                    [:li (post->link post)])
                  post-list)]]))

(def posts
  [{:id "hyperscript-lichess-tv"
    :title "Making a Lichess TV Viewer with Hyperscript"
    :icon 'chess-knight
    :author elias-prescott
    :tags #{'hyperscript 'chess}
    :created (jt/local-date 2026 04 16)
    :body [[:p
            [:a {:href "https://hyperscript.org"} "Hyperscript"]
            " is an alternative scripting language for the web that optimizes for clean syntax and elegant interactions with the browser DOM APIs."
            " It is very pleasant to work in. I had to bounce back and force between my editor and the language documentation, but I got the hang"
            " of it pretty quick. I won't be reaching for it all the time, but for hobby projects, it is pretty fun."]
           [:p "To kick the tires on it, I made "
            [:a {:href "https://eliasprescott.github.io/lichess-tv"} "Lichess TV"]
            " for watching live Chess games."
            ""]
           [:p "My favorite piece of Hyperscript that I wrote is this snippet for fetching the Lichess TV API route and sending the game updates to another element as custom events."]
           [:code [:pre
"init
  fetch https://lichess.org/api/tv/feed as Response
  set $stream to it's body
  for x in $stream
    send fen(update: parseChunk(x)) to #board
  end
end"]]
           [:p "Maybe it's not practical for everyday use, but it does feel nice to write..."]]}])

(s/def :post/id string?)
(s/def :post/title string?)
(s/def :post/author string?)
(s/def :post/tags set?)
(s/def :post/body any?)
(s/def :post/created jt/local-date?)
(s/def :post/icon symbol?)
(s/def :blog/post (s/keys :req-un [:post/id :post/title :post/author :post/created :post/tags :post/body :post/icon]))

(def all-tags
  (reduce clojure.set/union (map :tags posts)))

(def posts-by-tag
  (->> all-tags
       (map (fn [tag]
              [tag (vec (filter (fn [post] (-> post :tags (tag)))
                                posts))]))
       (into {})))

(def home-page
  (base-template
    "Home Page"
    [[:p "Hi! My name is Elias Prescott. I like to learn things and sometimes write about them."]
     [:h2 "All posts"]
     [:ul (map (fn [p] [:li (post->link p)])
               posts)]

     [:h2 "Posts by tag"]
     `[:ul
       ~@(vec (map (fn [[tag post-list]]
                     [:li [:a {:href (tag->url tag)}
                           tag " (" (count post-list) ")"]])
                   posts-by-tag))]]))

(defmacro sh
  [& args]
  `(assert (= 0 (:exit (clojure.java.shell/sh ~@args)))))

(defn -main
  [& args]
  (sh "rm" "-rf" "_build/")
  (.mkdirs (io/file "_build"))
  (println "Copying static directory")
  (sh "cp" "-R" "static/" "_build/static")
  (println "Building home page")
  (spit "_build/index.html" (str (h/html home-page)))
  (println "Building post pages")
  (for [post posts]
    (do
      (.mkdirs (io/file (str "_build/posts/" (:id post))))
      (spit (str "_build/posts/" (:id post) "/index.html") (str (h/html (post-page-template post))))))
  (println "Building tag pages")
  (for [[tag post-list] posts-by-tag]
    (do
      (.mkdirs (io/file (str "_build/tags/" tag)))
      (spit (str "_build/tags/" tag "/index.html")
            (str (h/html (tags-page-template tag post-list))))))
  (println "Done!")
  (shutdown-agents))
