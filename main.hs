{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, OverloadedStrings, OverloadedLists #-}

import Prelude hiding (head, div, span)
import Data.Text (Text)
import Data.Text qualified as T
import Data.String (fromString)
import Data.Map (Map, empty)
import Data.Map qualified as M
import Data.List qualified as L
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (Day, showGregorian, fromGregorian)
import System.FilePath qualified as FilePath
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.Directory qualified as Dir
import System.Posix qualified as Posix
import qualified System.Directory as Dir
import qualified System.Posix as Posix
import Control.Monad (when)

data HtmlItem =
  Element { selfClosing :: Bool, tag :: Text, attrs :: Map Text Text, body :: [HtmlItem] }
  | Text Text
  | Raw { content :: Text }

escapeHtmlStr =
  T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"

escapeHtml =
  T.replace "&" "&amp;"
  . T.replace "<" "&lt;"
  . T.replace ">" "&gt;"
  . T.replace "\"" "&quot;"
  . T.replace "'" "&#39;"

attrToHtml (key, val) =
  -- using the full escapeHtml is overzealous here because most modern HTML parsers
  -- don't need full escaping within attribute values
  T.concat [ escapeHtml key, "=\"", escapeHtml val, "\"" ]

attrsToHtml = T.concat . L.intersperse " " . map attrToHtml . M.toList

elemToHtml (Raw content) = content
elemToHtml (Text text) = escapeHtmlStr text
elemToHtml (Element selfClosing tag attrs body) =
  T.concat [
    "<", tag,
    if not $ M.null attrs then T.concat [" ", attrsToHtml attrs]
    else "",
    if selfClosing then "/>"
    else T.concat [">", T.concat (map elemToHtml body), "</", tag, ">"]
  ]

html = Element False "html"
head = Element False "head"
title = Element False "title"
meta attrs = Element True "meta" attrs []
link attrs = Element True "link" attrs []
body = Element False "body"
_main = Element False "main"
header = Element False "header"
sub = Element False "sub"
time = Element False "time"
br = Element True "br" [] []
hr = Element True "hr" [] []
h1 = Element False "h1"
h2 = Element False "h2"
h3 = Element False "h3"
h4 = Element False "h4"
h5 = Element False "h5"
h6 = Element False "h6"
a = Element False "a"
chip = Element False "chip"
div = Element False "div"
strong = Element False "strong"
p = Element False "p"
img attrs = Element True "img" attrs []
iframe = Element False "iframe"
code = Element False "code"
pre = Element False "pre"
nav = Element False "nav"
footer = Element False "footer"
ol = Element False "ol"
ul = Element False "ul"
li = Element False "li"
span = Element False "span"
figure = Element False "figure"
blockquote = Element False "blockquote"
figcaption = Element False "figcaption"
cite = Element False "cite"

quotation quoteBody quoteCaption =
  figure [] [
    blockquote [] quoteBody,
    figcaption [] [
      cite [] quoteCaption
    ]
  ]

siteTitle = "australorp.dev"
eliasPrescott = "Elias Prescott"

data Post = Post {
  id :: Text,
  title :: Text,
  icon :: Text,
  author :: Text,
  created :: Day,
  body :: [HtmlItem],
  tags :: Set Text
}

data Resource = Resource {
  id :: Text,
  title :: Text,
  icon :: Text,
  updated :: Day,
  body :: [HtmlItem]
}

postUrl p = T.concat [ "/posts/", p.id ]
postLink p = do
  iconSvg <- icon p.icon
  return $ a [("href", postUrl p)] [
      iconSvg,
      Text p.title
    ]

tagUrl t = T.concat ["/tags/", t]

resourceUrl r = T.concat ["/resources/", r.id]
resourceLink r = do
  iconSvg <- icon r.icon
  return $ a [("href", resourceUrl r)] [
    iconSvg,
    Text r.title
    ]

posts :: [Post]
posts = [
  Post {
    id = "professional-programmer-talk",
    title = "Being a Professional Programmer",
    icon = "user",
    author = eliasPrescott,
    tags = ["programming"],
    created = fromGregorian 2026 5 4,
    body = [
      p [] [
        Text "These are notes for a presentation I am giving on being a professional programmer. ",
        Text "I have tweaked the spacing of some elements for ease of reading on a whiteboard."
      ],
      h2 [] [Text "Soft Skills"],
      div [("class", "presentation")] [
        ul [] [
          li [] [
            Text "Caring for people",
            ul [] [
              li [] [Text "Every job is people-oriented. All work ultimately is about serving people."],
              li [] [Text "Showing people that you care about them is one of the most important things you can do in work or life in general."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Attention to detail",
            ul [] [
              li [] [Text "Attention to detail is \"the act or state of applying the mind to something.\""],
              li [] [
                Text "Practical applications:",
                ul [] [
                  li [] [Text "Active concentration on work."],
                  li [] [Text "Good note-taking (Lookup \"zettlekasten\". Look into tools like Obsidian or LoqSeq.)"],
                  li [] [Text "Studying the problem domain or documentation (more on this later)."]
                ]
              ]
            ]
          ],
          br,
          br,
          li [] [
            Text "Connecting with people through small talk or humor",
            ul [] [
              li [] [Text "Not easy for most programmers, but it's important for any job."],
              li [] [Text "Second to caring, this is the best way to get people to open up."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Good writing skills",
            ul [] [
              li [] [Text "Lots of communication happens through writing (email, messages, technical documentation)."],
              li [] [Text "\"Besides a mathematical inclination, an exceptionally good mastery of one's native tongue is the most vital asset of a competent programmer.\" - Edsger Dijkstra"]
            ]
          ],
          br,
          br,
          li [] [
            Text "Passion for the work",
            ul [] [
              li [] [Text "This has been one of the best skills for my career."],
              li [] [Text "This skill motivates me to develop all the other skills."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Ability to learn new things",
            ul [] [
              li [] [Text "IT as an industry changes incredibly quickly."],
              li [] [Text "There will always be new technologies to evaluate and learn, but very few of them are worth learning."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Good reading comprehension",
            ul [] [
              li [] [Text "As a programmer, I read a ton of technical content (articles, code, documentation) each week."],
              li [] [Text "Skimming is a valuable skill."],
              li [] [Text "Read to get a general overview, and then come back whenever you have a specific need."]
            ]
          ]
        ],

        hr,

        h2 [] [Text "Other Topics"],
        ul [] [
          li [] [
            Text "Web Development",
            ul [] [
              li [] [Text "Web development was my entry into the industry (I got a part-time job in it in college)."],
              li [] [Text "It is probably the easiest way to enter the industry because some web dev jobs have a low barrier to entry."],
              li [] [Text "You can specialize in frontend or backend only, but I prefer full-stack."],
              li [] [Text "Don't focus on frameworks, focus on the medium: HTML, CSS, JavaScript, SQL."],
              li [] [Text "But, also learn some popular frameworks because they are useful and good for your resume."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Becoming a subject matter expert",
            ul [] [
              li [] [Text "Working as a programmer is rarely just about programming."],
              li [] [Text "As a programmer, you are in a unique position to learn and leverage business knowledge."],
              li [] [Text "Understanding how the business works can inform lots of technical decisions."],
              li [] [Text "It also helps you comprehend and work with the business' data."],
              li [] [Text "Being a good learner is essential here. I have recently been learning a lot about accounting because I am working on our app's accounting features."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Being a Specialist vs a Generalist",
            ul [] [
              li [] [Text "IT is such a large field. Specializing in a specific area can help you focus on mastering a smaller subset of skills."],
              li [] [Text "Large companies have a need for specialists, but smaller companies typically want generalists."],
              li [] [Text "I would recommend being a generalist unless someone is paying you to specialize."],
              li [] [Text "I have learned so much from being open to new tasks or responsibilities. Being a generalist lets me constantly work on new domains or new types of problems."],
              li [] [Text "The hardest problems cross multiple layers of an application (frontend, backend, database, etc.), so being a generalist and having holistic knowledge puts you in the best position to solve difficult problems."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Development Workflow",
            ul [] [
              li [] [Text "Knowing your tools can be a huge boost to productivity and makes work more fun."],
              li [] [Text "I like using a non-standard editor (NeoVim), but learning some advanced shortcuts will make your experience better with any editor."],
              li [] [Text "This ties in with the Unix philosophy where text-based tools start to compose, so your knowledge of different tools can compound."],
              li [] [Text "Learning common unix/posix terminal utilities can be a big productivity boost."],
              li [] [Text "Learning a little Bash (the shell & the scripting language) goes a long way. If you are on Windows, you can still use Git Bash for a similar experience."],
              li [] [
                Text "The easiest wins:",
                ul [] [
                  li [] [Text "Shortcuts to run/build your project."],
                  li [] [Text "Find a way to visually surface errors without tons of context switching (terminal or an in-editor LSP)."],
                  li [] [Text "Shortcuts to switch between windows (lookup \"tiling window managers\" to get an idea)."],
                  li [] [Text "If you are in web development, learn to use the browser's DevTools! I use the DevTools on almost every bug I triage."],
                  li [] [Text "Learn Git or your VCS of choice well. Use a visual tool to make Git easier (lazygit, magit, or whatever VSCode plugin is popular these days)."]
                ]
              ]
            ]
          ],
          br,
          br,
          li [] [
            Text "Functional Programming",
            ul [] [
              li [] [Text "Functional programming is an alternative paradigm to object-oriented programming (OOP)."],
              li [] [Text "Neither paradigm is the be-all and end-all, but learning both is beneficial."],
              li [] [
                Text "Generally, functional programming includes:",
                ul [] [
                  li [] [Text "separating data from behavior,"],
                  li [] [Text "avoiding mutation and restricting side effects,"],
                  li [] [Text "using algebraic data types (records, discriminated unions),"],
                  li [] [Text "using recursion over iteration."]
                ]
              ],
              li [] [Text "If you have used C# before, F# is a great introduction to functional programming."],
              li [] [Text "If you are feeling more adventerous, OCaml and Haskell can be lots of fun, but their tooling and ecosystems have some rough edges."]
            ]
          ],
          br,
          br,
          li [] [
            Text "Modern Low-Level Programming Languages",
            ul [] [
              li [] [Text "I just mentioned F#, OCaml, and Haskell, but there are other languages that I think are worth trying."],
              li [] [Text "If you are looking for an alternative to C/C++, Odin and Zig are lots of fun."],
              li [] [Text "I especially love Odin because it's very pragmatic and elegantly designed."],
              li [] [Text "Odin also comes bundled with some third-party dependencies like Raylib, so making simple games with it is really easy."]
            ]
          ]
        ]
      ]
    ]
  },
  Post {
    id = "hyperscript-lichess-tv",
    title = "Making a Lichess TV Viewer with Hyperscript",
    icon = "chess-knight",
    author = "Elias Prescott",
    tags = ["chess", "hyperscript"],
    created = fromGregorian 2026 4 16,
    body = [
      p [] [
        a [("href", "https://hyperscript.org")] [Text "Hyperscript"],
        Text " is an alternative scripting language for the web that optimizes for clean syntax and elegant interactions with the browser DOM APIs. ",
        Text "It is very pleasant to work in. I had to bounce back and force between my editor and the language documentation, but I got the hang of it pretty quickly."
      ],
      p [] [
        Text "To kick the tires on it, I made ",
        a [("href", "https://eliasprescott.github.io/lichess-tv")] [Text "Lichess TV"],
        Text " for watching live chess games. "
      ],
      p [] [
        Text "My favorite piece of Hyperscript that I wrote is this snippet for fetching the Lichess TV API route and sending the game updates to another element as custom events.",
        code [] [pre [] [Text $ T.concat [
        "init\n",
        "  fetch https://lichess.org/api/tv/feed as Response\n",
        "  set $stream to it's body\n",
        "  for x in $stream\n",
        "    send fen(update: parseChunk(x)) to #board\n",
        "  end\n",
        "end\n"
        ]]]
      ],
      p [] [Text "Maybe it's not the most practical for everyday use, but it does feel nice to write..."]
    ]
  }, Post {
    id = "godot-virtual-joystick",
    title = "Godot Virtual Joystick",
    icon = "joystick",
    author = eliasPrescott,
    tags = ["godot"],
    created = fromGregorian 2026 04 22,
    body = [
      p [] [
        Text "Godot version 4.7 dev 1 introduced a new UI node: the ",
        a [("href", "https://godotengine.org/article/dev-snapshot-godot-4-7-dev-1/#input-virtualjoystick")] [Text "Virtual Joystick."]
      ],
      p [] [
        Text "I was able to add a virtual joystick to the ",
        a [("href", "https://docs.godotengine.org/en/stable/getting_started/first_2d_game/index.html")] [Text "2D intro tutorial"],
        Text " with very little effort. ",
        Text "If you want to try it out, you can play it ",
        a [("href", "https://eliasprescott.github.io/godot-demo/")] [Text "here"],
        Text " or you can try playing it in the iframe below."
      ],
      p [] [
        iframe [("src", "https://eliasprescott.github.io/godot-demo/"),
                ("style", "width: 100%; aspect-ratio: 1 / 1.5; max-height: 90vh;")] []
      ]
    ]
  }]

postsByDate = L.sortBy (\x y -> compare y.created x.created) posts

tags = L.nub $ L.concatMap (\x -> Set.toList x.tags) posts
postsByTag = M.fromList $ L.map (\tag -> (tag, L.filter (\post -> Set.member tag post.tags) posts)) tags

newtype LinkTree = LinkT (Text, Maybe Text, [LinkTree])

programmingDocumentationLinks :: [LinkTree]
programmingDocumentationLinks = [
  LinkT ("GNU Bash", Just "https://www.gnu.org/software/bash/manual/", [
    LinkT ("Looping Constructs", Just "https://www.gnu.org/software/bash/manual/bash.html#Looping-Constructs", []),
    LinkT ("Shell Parameter Expansion", Just "https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion", [])
    ]),
  LinkT ("Postgres", Just "https://www.postgresql.org/docs/", [
    LinkT ("Window Function Calls", Just "https://www.postgresql.org/docs/current/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS", []),
    LinkT ("JSON Functions and Operators", Just "https://www.postgresql.org/docs/current/functions-json.html", []),
    LinkT ("PL/pgSQL - SQL Procedural Language", Just "https://www.postgresql.org/docs/current/plpgsql.html", []),
    LinkT ("Postgres Wiki", Just "https://wiki.postgresql.org/wiki/Main_Page", [
      LinkT ("Don't Do This (Common Postgres Mistakes)", Just "https://wiki.postgresql.org/wiki/Don%27t_Do_This", [])
      ]),
    LinkT ("psql (PostgreSQL interactive terminal)", Just "https://www.postgresql.org/docs/current/app-psql.html", [])
    ]),
  LinkT ("Odin", Just "https://odin-lang.org/docs/overview/", [
    LinkT ("string type conversions", Just "https://odin-lang.org/docs/overview/#string-type-conversions", [])
    ]),
  LinkT ("Nix Flakes", Just "https://nixos.wiki/wiki/Flakes", [
    LinkT ("Flake schema", Just "https://nixos.wiki/wiki/Flakes#Flake_schema", [])
    ])
  ]

orderLinkTreesByText :: [LinkTree] -> [LinkTree]
orderLinkTreesByText = L.sortBy (\(LinkT (x, _, _)) (LinkT (y, _, _)) -> compare x y)

linkTreeToElementText txt (Just href) =
  a [("href", href)] [Text txt]
linkTreeToElementText txt Nothing =
  span [] [Text txt]

linkTreeToElementChildren [] =
  []
linkTreeToElementChildren children = [
  ul [] $ map linkTreeToElement (orderLinkTreesByText children)
  ]

linkTreeToElement :: LinkTree -> HtmlItem
linkTreeToElement (LinkT (txt, maybeHref, children)) =
  li [] $ linkTreeToElementText txt maybeHref : linkTreeToElementChildren children

resources :: [Resource]
resources = [
  Resource {
    id = "programming",
    title = "Programming",
    icon = "binary",
    updated = fromGregorian 2026 4 27,
    body = [
      h2 [] [Text "Documentation Shortcuts"],
      ul [] $ map linkTreeToElement (orderLinkTreesByText programmingDocumentationLinks),

      h2 [] [Text "Software Design"],
      h3 [] [Text "Videos"],
      ul [] [
        li [] [a [("href", "https://youtu.be/SxdOUGdseq4")] [Text "Simple Made Easy - Rich Hickey (2011)"]]
      ],
      h3 [] [Text "Essays"],
      ul [] [
        li [] [a [("href", "https://htmx.org/essays/codin-dirty")] [Text "Codin' Dirty - Carson Gross (2024)"]],
        li [] [a [("href", "https://grugbrain.dev")] [Text "Grug Brain Developer - Carson Gross (2022)"]],
        li [] [a [("href", "https://caseymuratori.com/blog_0015")] [Text "Semantic Compression - Casey Muratori (2014)"]],
        li [] [a [("href", "https://www.gingerbill.org/article/2020/05/31/programming-pragmatist-proverbs/")] [Text "Pragmatism in Programming Proverbs - Ginger Bill (2020)"]]
      ],
      h3 [] [Text "Books"],
      ul [] [
        li [] [Text "A Philosophy of Software Design - John Ousterhout (2018)"],
        li [] [Text "The Mythical Man Month - Fred Brooks (1975)"],
        li [] [a [("href", "https://aosabook.org/en/")] [
          Text "The Architecture of Open Source Applications - Edited by Amy Brown & Greg Wilson"
        ]]
      ],

      h2 [] [Text "Quotes"],
      h3 [] [Text "On Complexity"],
      quotation
        [Text "The competent programmer is fully aware of the strictly limited size of his own skull; therefore he approaches the programming task in full humility, and among other things he avoids clever tricks like the plague."]
        [a [("href", "https://en.wikiquote.org/wiki/Edsger_W._Dijkstra")] [Text "Edsger Dijkstra"]],
      quotation
        [Text "Simplicity is prerequisite for reliability."]
        [a [("href", "https://en.wikiquote.org/wiki/Edsger_W._Dijkstra")] [Text "Edsger Dijkstra"]],
      quotation
        [Text "There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies, and the other way is to make it so complicated that there are no obvious deficiencies. The first method is far more difficult."]
        [a [("href", "https://en.wikiquote.org/wiki/C._A._R._Hoare")] [Text "Tony Hoare"]]
    ]
  }]

icon name = do
  svg <- readFile (T.unpack (T.concat ["icons/", name, ".svg"]))
  return $ Raw $ T.pack svg

breadcrumbToLink (href, text, current) =
  let currentAttrs = if current then [("aria-current", "")] else M.empty in
  li [] [a (M.union [("href", href)] currentAttrs) [Text text]]

breadcrumbs items =
  let crumbLinks = map breadcrumbToLink items in
  nav [("class", "breadcrumbs"), ("aria-label", "Breadcrumbs")] [
    ol [] crumbLinks
  ]

baseTemplate :: Text -> [HtmlItem] -> IO HtmlItem
baseTemplate pageTitle pageBody = do
  xmlIcon <- icon "code-xml"
  contactIcon <- icon "contact"
  return $ html [] [
      head [] [
        title [] [Text $ T.concat [pageTitle, " | ", siteTitle]],
        meta [("name", "viewport"), ("content", "width=device-width")],
        link [("rel", "stylesheet"), ("href", "/static/missing.min.css")],
        link [("rel", "stylesheet"), ("href", "/static/styles.css")]
      ],
      body [] [
        header [] [
          h1 [] [a [("href", "/")] [Text siteTitle]],
          nav [("class", "tool-bar")] [
            a [("class", "flex-row align-items:center crowded"), ("href", "https://github.com/EliasPrescott")] [
              Text "GitHub",
              xmlIcon
            ],
            a [("class", "flex-row align-items:center crowded"), ("href", "https://www.linkedin.com/in/elias-prescott")] [
              Text "LinkedIn",
              contactIcon
            ]
          ]
        ],
        _main [] pageBody,
        footer [] [
          p [] [
            Text "Made with ",
            a [("href", "https://haskell.org")] [Text "Haskell"],
            Text " and ",
            a [("href", "https://missing.style")] [Text "missing.css"],
            Text "."
          ],
          p [("class", "flex-row crowded")] [
            img [("src", "/static/dark-mode.gif")],
            img [("src", "/static/neovim.gif")]
          ]
        ]
      ]
    ]

postPageTemplate :: Post -> IO HtmlItem
postPageTemplate post = baseTemplate post.title $ [
  breadcrumbs [("/", "Home", False),
               (postUrl post, T.concat ["Post: ", post.title], True)],
  h1 [] [Text post.title],
  sub [] [
    Text $ T.concat [
      "Written by ",
      post.author,
      " on ",
      T.pack $ showGregorian post.created
    ]
  ],
  br,
  br,
  div [("class", "flex-row crowded")] $ strong [] [Text "Tags:"]
    : map (\tag -> chip [] [a [("href", tagUrl tag)] [Text tag]]) (Set.toList post.tags)
  ] ++ post.body

resourcePageTemplate r =
  let title = T.concat [r.title, " Resources"] in
  baseTemplate title $ [
    breadcrumbs [("/", "Home", False),
                 (resourceUrl r, T.concat ["Resources: ", r.title], True)],
    h1 [] [Text title],
    sub [] [
      Text "Last updated ",
      time [] [Text $ T.pack (showGregorian r.updated)]
    ],
    p [] [
      Text "This is a collection of resources on the topic of programming. I will continually update this page as I find more resources that I like."
    ]
  ] ++ r.body

tagPageTemplate t posts = do
  postLinks <- mapM postLink posts
  baseTemplate t [
    breadcrumbs [("/", "Home", False),
                 (tagUrl t, T.concat ["Tag: ", t], True)],
    h1 [] [Text $ T.concat [t, " posts (", (T.pack . show . length) posts, ")"]],
    ul [] $ map (\x -> li [] [x]) postLinks
    ]

homePage = do
  postLinks <- mapM postLink postsByDate
  resourceLinks <- mapM resourceLink resources
  baseTemplate "Home Page" [
    Text "Hi! My name is Elias Prescott. I like to learn things and sometimes write about them.",

    h2 [] [Text "Resources"],
    ul [] $ map (\x -> li [] [x]) resourceLinks,

    h2 [] [Text "All posts"],
    ul [] $ map (\x -> li [] [x]) postLinks,

    h2 [] [Text "Posts by tag"],
    ul [] $ map (\(tag, posts) -> li [] [
      a [("href", tagUrl tag)] [
        Text $ T.concat [tag, " (", (T.pack . show . length) posts, ")"]
      ]
    ]) $ M.toList postsByTag
    ]

buildDir = "_build/"
postsDir = FilePath.combine buildDir "posts/"
resourcesDir = FilePath.combine buildDir "resources/"
tagsDir = FilePath.combine buildDir "tags/"
staticDir = FilePath.combine buildDir "static/"

-- TODO: unify all the different types of compiled pages into an intermediate page type so I can reuse this logic
-- and add more advanced features later on like cross-reference tracking
compilePost post = do
  let postId = T.unpack post.id
  let postDir = FilePath.combine postsDir postId
  createDirectoryIfMissing True postDir
  htmlItem <- postPageTemplate post
  let html = T.unpack $ elemToHtml htmlItem
  let indexFilePath = FilePath.combine postDir "index.html"
  writeFile indexFilePath html

compileResource r = do
  let resourceId = T.unpack r.id
  let resourceDir = FilePath.combine resourcesDir resourceId
  createDirectoryIfMissing True resourceDir
  htmlItem <- resourcePageTemplate r
  let html = T.unpack $ elemToHtml htmlItem
  let indexFilePath = FilePath.combine resourceDir "index.html"
  writeFile indexFilePath html

compileTagPage t posts = do
  let tagDir = FilePath.combine tagsDir $ T.unpack t
  createDirectoryIfMissing True tagDir
  htmlItem <- tagPageTemplate t posts
  let html = T.unpack $ elemToHtml htmlItem
  let indexFilePath = FilePath.combine tagDir "index.html"
  writeFile indexFilePath html

compileHomePage = do
  createDirectoryIfMissing True buildDir
  htmlItem <- homePage
  let html = T.unpack $ elemToHtml htmlItem
  let filePath = FilePath.combine buildDir "index.html"
  writeFile filePath html

copyDirRecursive srcDir destDir = do
  paths <- Dir.listDirectory srcDir
  mapM_ (\path -> do
    let srcPath = FilePath.combine srcDir path
    let destPath = FilePath.combine destDir path
    status <- Posix.getFileStatus srcPath
    if Posix.isDirectory status then copyDirRecursive srcPath destPath
    else if Posix.isRegularFile status then do
      createDirectoryIfMissing True destDir
      Dir.copyFile srcPath destPath
    else pure ()) paths

main = do
  buildDirExist <- Dir.doesDirectoryExist buildDir
  when buildDirExist $ Dir.removeDirectoryRecursive buildDir
  putStrLn "removing existing build dir"
  compileHomePage
  putStrLn "compiled home page"
  mapM_ compilePost posts
  putStrLn $ "compiled " ++ show (length posts) ++ " posts"
  mapM_ compileResource resources
  putStrLn $ "compiled " ++ show (length resources) ++ " resource pages"
  mapM_ (uncurry compileTagPage) (M.toList postsByTag)
  putStrLn $ "compiled " ++ show (length tags) ++ " tag pages"
  copyDirRecursive "static/" staticDir
  putStrLn "copied static/ directory"
