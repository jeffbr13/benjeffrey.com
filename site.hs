--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.Pandoc.Options as Pandoc.Options


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        {   deployCommand = "./deploy.sh"}

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.Options.writerHtml5 = True
                        , Pandoc.Options.writerHtmlQTags = True
                        --, Pandoc.Options.writerNumberSections = True
                        --, Pandoc.Options.writerNumberOffset = [1]
                        , Pandoc.Options.writerSectionDivs = True
                        , Pandoc.Options.writerTableOfContents = True
                    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match "templates/*" $ compile templateCompiler

    -- copy site icon to `favicon.ico`
    match "static/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler

    -- public GPG key
    match "static/mail@benjeffrey.com.asc" $ do
        route   (constRoute "mail@benjeffrey.com.asc")
        compile copyFileCompiler

    match "static/vCard.vcf" $ do
        route   (constRoute "vCard.vcf")
        compile copyFileCompiler

    match "static/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scss/app.scss" $do
        route   $ gsubRoute "scss/" (const "static/css/") `composeRoutes` setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
            >>= return . fmap compressCss

    -- copy humans.txt and robots.txt to web root
    match (fromList ["humans.txt", "robots.txt"]) $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile static pages to web root with Pandoc
    match (fromList ["cv.md"]) $ do
        route   $ setExtension ""
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/generic.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension ""
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
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
                >>= loadAndApplyTemplate "templates/base.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                                postList $ fmap (take 3) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" postCtx
                >>= relativizeUrls


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
