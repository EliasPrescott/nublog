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
        li [] [a [("href", "https://caseymuratori.com/blog_0015")] [Text "Semantic Compression - Casey Muratori (2014)"]]
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
