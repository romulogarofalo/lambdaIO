{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Post where

import Import
import Database.Persist.Postgresql


postPostR :: Handler Value
postPostR = do
    post <- requireJsonBody :: Handler Post
    pid <- runDB $ insert post
    sendStatusJSON created201 (object ["id" .= fromSqlKey pid])

getPostR :: Handler Value
getPostR = do
    dt <- runDB $ selectList [] [Asc PostId]
    sendStatusJSON ok200 (object ["content" .= dt])

putPostSpecificR :: PostId -> Handler Value
putPostSpecificR idPost = do
    _ <- runDB $ get404 idPost
    newPost <- requireJsonBody :: Handler Post
    runDB $ replace idPost newPost
    sendStatusJSON noContent204 (object [])

patchPostLike :: PostId -> Int -> Handler Value
patchPostLike idPost likes = do
    _ <- runDB $ get404 idPost
    runDB $ update idPost [PostLikes =. likes]
    sendStatusJSON noContent204 (object [])

deletePostSpecificR :: PostId -> Handler Value
deletePostSpecificR idPost = do
    _ <- runDB $ get404 idPost
    runDB $ delete idPost
    sendStatusJSON noContent204 (object [])
