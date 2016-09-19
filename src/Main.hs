{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))

import Hakyll

-- Don't forget to stack build before hakyll building.
--
-- Perhaps I should break out some stuff into other files/modules.

main :: IO ()
main = hakyll $ do

    match "templates/*" $ compile templateCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match ("404.html" .||. "about.html" .||. "projects.html") $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext

    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/posts.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= loadAndApplyTemplate "templates/boilerplate.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexContext = listField "posts" defaultContext (return posts) <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= loadAndApplyTemplate "templates/boilerplate.html" indexContext
                >>= relativizeUrls
                
    match "archive.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexContext = listField "posts" defaultContext (return posts) <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= loadAndApplyTemplate "templates/boilerplate.html" indexContext
                >>= relativizeUrls
