--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        {   deployCommand = "scp -rC ./_site/* parsley:/var/www/benjeffrey.com/ && scp -C ./nginx parsley:/etc/nginx/sites_enabled/benjeffrey.com"}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match "images/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scss/app.scss" $do
        route   $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"]) >>=
                return . fmap compressCss

    --match "css/*" $ do
    --    route   idRoute
    --    compile compressCssCompiler

    -- static files
    match (fromList ["humans.txt", "robots.txt"]) $ do
        route   idRoute
        compile copyFileCompiler

    -- Static webpages compiled through Pandoc
    match (fromList ["cv.md"]) $ do
        --route   $ setExtension "html"
        route   $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/generic.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        --route $ setExtension "html"
        route $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst)          `mappend`
                    constField "title" "Posts archive"                   `mappend`
                    constField "description" "Previous posts on benjeffrey.com"  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 3) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/generic.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
