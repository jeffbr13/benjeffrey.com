--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import qualified Text.Pandoc.Options as Pandoc.Options


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        {   deployCommand = "src/deploy.sh"}

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.Options.writerHtml5 = True
                        , Pandoc.Options.writerHtmlQTags = True
                        --, Pandoc.Options.writerNumberSections = True
                        --, Pandoc.Options.writerNumberOffset = [1]
                        , Pandoc.Options.writerSectionDivs = True
                        , Pandoc.Options.writerTableOfContents = True
                    }

postsDirectory :: Pattern
postsDirectory = "content/posts/**"
--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match "templates/*" $ compile templateCompiler


    -- compile CV:
    match (fromList ["content/cv.md"]) $ do
        route   $ setExtension "" `composeRoutes` gsubRoute "content/" (const "")
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/generic.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls


    -- compile posts:
    match postsDirectory $ do
        route $ setExtension "" `composeRoutes` gsubRoute "content/" (const "")
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls


    -- build index page:
    match "content/index.html" $ do
        route (gsubRoute "content/" (const ""))
        compile $ do
            let indexCtx = field "posts" (\_ -> postList recentFirst)
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" postCtx
                >>= relativizeUrls


    -- copy other site content:
    match "content/**" $ do
        route (gsubRoute "content/" (const ""))
        compile copyFileCompiler

    -- copy Zurb Foundation icon fonts into fonts directory
    match "bower_components/foundation-icon-fonts/foundation-icons*" $ do
        route (gsubRoute "bower_components/foundation-icon-fonts/" (const "fonts/"))
        compile copyFileCompiler

    -- compile SCSS:
    match "src/scss/main.scss" $do
        route   $ constRoute "stylesheet.css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s",
                                                 "--scss",
                                                 "--compass",
                                                 "--style", "compressed",
                                                 "--load-path", "src/scss"])
            >>= return . fmap compressCss


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll postsDirectory
    itemTpl <- loadBody "templates/post-list-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
