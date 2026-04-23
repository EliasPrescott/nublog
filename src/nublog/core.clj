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

(defn quotation
  [body caption]
  [:figure
   [:blockquote body]
   [:figcaption
    [:cite caption]]])

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

(defn resource->url
  [r]
  (str "/resources/" (:id r)))

(defn post->link
  [p]
  [:a {:href (post->url p)}
   (icon (:icon p))
   (:title p)])

(defn resource->link
  [r]
  [:a {:href (resource->url r)}
   (icon (:icon r))
   (:title r)])

(defn tag->url
  [t]
  (str "/tags/" t))

(defn base-template
  [title body]
  [:html
   [:head
    [:title (str title " | " site-title)]
    [:meta {:name "viewport"
            :content "width=device-width"}]
    [:link {:rel "stylesheet"
            :href "/static/missing.min.css"}]
    [:link {:rel "stylesheet"
            :href "/static/styles.css"}]
    [:link {:rel "icon"
            :href "/favicon.svg"
            :type "image/svg+xml"}]]
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
     [:p
       "Made with "
       [:a {:href "https://clojure.org"} "Clojure"]
       " and "
       [:a {:href "https://missing.style"} "missing.css"]
       "."]
     [:div.flex-row.crowded
      [:img {:src "/static/neovim.gif"}]
      [:img {:src "/static/dark-mode.gif"}]]]]])

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

(defn resources-page-template
  [resource]
  (let [title (str (:title resource) " Resources")]
    (base-template title
      [(breadcrumbs
         [["/" "Home"]
          [(resource->url resource) (str "Resources: " (:title resource))]])
       [:h1 title]
       [:sub "Last updated " [:time (:last-updated resource)]]
       [:p
        "This is a collection of resources on the topic of " (:id resource)". "
        "I will continually update this page as I find more resources that I like."]
       [:hr]
       (seq (:body resource))])))

(def resources
  [{:id "programming"
    :title "Programming"
    :icon 'binary
    :last-updated (jt/local-date 2026 04 22)
    :body
    [[:h2 "Documentation Shortcuts"]
     [:ul
      [:li [:a {:href "https://www.gnu.org/software/bash/manual/"}
            "GNU Bash"]
       [:ul
        [:li [:a {:href "https://www.gnu.org/software/bash/manual/bash.html#Looping-Constructs"}
              "Looping Constructs"]]
        [:li [:a {:href "https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion"}
              "Shell Parameter Expansion"]]]]
      [:li [:a {:href "https://www.postgresql.org/docs/"}
            "Postgres"]
       [:ul
        [:li [:a {:href "https://www.postgresql.org/docs/current/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS"}
              "Window Function Calls"]]
        [:li [:a {:href "https://www.postgresql.org/docs/current/functions-json.html"}
              "JSON Functions and Operators"]]
        [:li [:a {:href "https://www.postgresql.org/docs/current/plpgsql.html"}
              "PL/pgSQL - SQL Procedural Language"]]
        [:li [:a {:href "https://wiki.postgresql.org/wiki/Main_Page"}
              "Postgres Wiki"]
         [:ul
          [:li [:a {:href "https://wiki.postgresql.org/wiki/Don%27t_Do_This"}
                "Don't Do This (Common Postgres Mistakes)"]]]]
        [:li [:a {:href "https://www.postgresql.org/docs/current/app-psql.html"}
              "psql (PostgreSQL interactive terminal)"]]]]
      [:li [:a {:href "https://odin-lang.org/docs/overview/"}
            "Odin (the programming language)"]
       [:ul
        [:li [:a {:href "https://odin-lang.org/docs/overview/#string-type-conversions"}
              [:code "string"] " type conversions"]]]]]

     [:h2 "Software Design"]
     [:h3 "Videos"]
     [:ul
      [:li [:a {:href "https://youtu.be/SxdOUGdseq4"}
            "Simple Made Easy - Rich Hickey (2011)"]]]
     [:h3 "Essays"]
     [:ul
      [:li [:a {:href "https://htmx.org/essays/codin-dirty"}
            "Codin \"Dirty\" - Carson Gross (2024)"]]
      [:li [:a {:href "https://grugbrain.dev"}
            "Grug Brain Developer - Carson Gross (2022)"]]
      [:li [:a {:href "https://caseymuratori.com/blog_0015"}
            "Semantic Compression - Casey Muratori (2014)"]]]

     [:h3 "Books"]
     [:ul
      [:li "A Philosophy of Software Design - John Ousterhout (2018)"]
      [:li "The Mythical Man Month - Fred Brooks (1975)"]
      [:li [:a {:href "https://aosabook.org/en/"} "The Architecture of Open Source Applications - Edited by Amy Brown & Greg Wilson"]]]

     [:h2 "Quotes"]

     [:h3 "On Complexity"]


     (quotation
      "The competent programmer is fully aware of the strictly limited size of his own skull; therefore he approaches the programming task in full humility, and among other things he avoids clever tricks like the plague."
      [:a {:href "https://en.wikiquote.org/wiki/Edsger_W._Dijkstra"}
       "Edsger Dijkstra"])

     (quotation
      "Simplicity is prerequisite for reliability."
      [:a {:href "https://en.wikiquote.org/wiki/Edsger_W._Dijkstra"}
       "Edsger Dijkstra"])

     (quotation
      "There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies, and the other way is to make it so complicated that there are no obvious deficiencies. The first method is far more difficult."
      [:a {:href "https://en.wikiquote.org/wiki/C._A._R._Hoare"}
       "Tony Hoare"])]}])

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
           [:p "Maybe it's not practical for everyday use, but it does feel nice to write..."]]}
   {:id "godot-virtual-joystick"
    :title "Godot Virtual Joystick"
    :icon 'joystick
    :author elias-prescott
    :tags #{'godot}
    :created (jt/local-date 2026 04 22)
    :body [[:p "Godot version " [:code "4.7 Dev 1"] " introduced a new UI node: the "
            [:a {:href "https://godotengine.org/article/dev-snapshot-godot-4-7-dev-1/#input-virtualjoystick"} "Virtual Joystick."]]
           [:p "The virtual joystick automatically handles touch input exactly the way you would expect, and it translates joystick movements into "
            "any arbitrary input actions that you specify. "
            "This makes it trivial to slap on a virtual joystick and hook it up to your existing movement input actions. "]
           [:p
            "I was able to add a virtual joystick to the "
            [:a {:href "https://docs.godotengine.org/en/stable/getting_started/first_2d_game/index.html"}
             "2D intro tutorial"]
            " with very little effort. "
            "If you want to try it out, you can play it "
            [:a {:href "https://eliasprescott.github.io/godot-demo/"}
             "here"]
            " or you can try playing it in the iframe below."]
           [:p [:iframe {:src "https://eliasprescott.github.io/godot-demo/"
                         :style "width: 100%; aspect-ratio: 1 / 1.5; max-height: 90vh;"}]]]}
   {:id "digital-handyman"
    :title "Digital Handyman"
    :icon 'hammer
    :author elias-prescott
    :tags #{'programming-philosophy}
    :created (jt/local-date 2026 04 22)
    :body [[:p
            "I have heard it said that programmers are not really engineers. "
            "Instead, we are digital plumbers, connecting programs and APIs like we are hooking up pipes and fixing leaks when they occur. "
            "I like the analogy, but after thinking over my experiences so far, I would say I'm more of a digital handyman."]
           [:p
            "Throughout my (relatively short) time as a programmer, I have worked on a wide variety of projects. "
            "I have worked on: "
            [:ul
             [:li "internal web apps"]
             [:li "marketing and apartment leasing websites"]
             [:li "redesigning an e-commerce site"]
             [:li "writing automated tests for web apps, desktop apps, and data APIs"]
             [:li "setting up and administrating on-premise CI/CD systems (Jenkins)"]
             [:li "managing cloud CI/CD pipelines (GCP and Terraform)"]
             [:li "developing SQL reports for data analytics and application use"]
             [:li "and currently, refactoring a legacy double-entry bookkeeping system"]]]
           [:p
            "To my knowledge, I've never said no to a project. "
            "I have been eager to take on any task assigned to me, even when it's something unfamiliar. "
            "And I want to stress that almost every single one of those projects I listed was unfamiliar to me when I started it. "
            "There were many times where things weren't working out and I felt uncomfortable. "
            "I didn't know what the right solution was and it made me feel like I wasn't good enough."]
           [:p
            "The times where I feel uncomfortable and inadequate have always been the greatest periods of growth in my career, because those are the times where I am pushing myself and learning something new. "
            "We all want some level of comfort in our lives, but if everything is always comfortable, then you will never learn anything new."]
           [:p
            "Pushing myself has not only helped me grow, but it has also benefited my employers. "
            "When you are on a small team, there is no room for specialization. "
            "If you aren't willing to step out of your comfort zone to take on a project, then there's a good chance it won't get done."]
           [:p
            "So, I would say I am proud to be a 'digital handyman,' because it means I am willing to do whatever it takes to get the job done. "
            "I never know what the future holds, but I have always enjoyed my work. "
            "Ultimately, my work is not about making great software. "
            "It is about improving the lives of my coworkers and my customers. "
            "Software is just a tool that I use to achieve that goal."]]}])

(s/def :post/id string?)
(s/def :post/title string?)
(s/def :post/author string?)
(s/def :post/tags set?)
(s/def :post/body any?)
(s/def :post/created jt/local-date?)
(s/def :post/icon symbol?)
(s/def :blog/post (s/keys :req-un [:post/id :post/title :post/author :post/created :post/tags :post/body :post/icon]))

(def posts-newest-first (reverse (sort-by :created posts)))

(def all-tags
  (sort (reduce clojure.set/union (map :tags posts))))

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

     [:h2 "Resources"]
     [:ul (map (fn [resource] [:li (resource->link resource)])
               resources)]

     [:h2 "All posts"]
     [:ul (map (fn [p] [:li (post->link p)])
               posts-newest-first)]

     [:h2 "Posts by tag"]
     `[:ul
       ~@(vec (map (fn [[tag post-list]]
                     [:li [:a {:href (tag->url tag)}
                           tag " (" (count post-list) ")"]])
                   posts-by-tag))]]))

(defmacro sh
  [& args]
  `(assert (= 0 (:exit (clojure.java.shell/sh ~@args)))))

(defn build-site
  []
  (sh "rm" "-rf" "_build/")
  (.mkdirs (io/file "_build"))
  (sh "cp" "-R" "static/" "_build/static")
  (spit "_build/index.html" (str (h/html home-page)))
  (spit "_build/favicon.svg" (slurp "icons/favicon.svg"))
  (dorun (for [resource resources
               :let [dir (io/file "_build/resources" (:id resource))]]
           (do
             (.mkdirs dir)
             (spit (io/file dir "index.html") (str (h/html (resources-page-template resource)))))))
  (dorun (for [post posts
               :let [dir (io/file "_build/posts" (:id post))]]
           (do
             (.mkdirs dir)
             (spit (io/file dir "index.html") (str (h/html (post-page-template post)))))))
  (dorun (for [[tag post-list] posts-by-tag
               :let [dir (io/file "_build/tags" (str tag))]]
           (do
             (.mkdirs dir)
             (spit (io/file dir "index.html")
                   (str (h/html (tags-page-template tag post-list))))))))

(defn -main
  [& args]
  (build-site)
  (shutdown-agents))
