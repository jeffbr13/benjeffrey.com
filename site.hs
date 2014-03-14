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


    -- compile CV:
    match (fromList ["content/cv.md"]) $ do
        route   $ setExtension "" `composeRoutes` gsubRoute "content/" (const "")
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/generic.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls


    -- compile posts:
    match "content/posts/*" $ do
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


    -- compile SCSS:
    match "scss/app.scss" $do
        route   $ constRoute "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
            >>= return . fmap compressCss


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "content/posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
