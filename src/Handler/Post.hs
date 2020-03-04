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

getPostPageR :: Handler Html
getPostPageR = do
    defaultLayout $ [whamlet|
        <div class="row">
            <div class="col-2">
                <img class="img-fluid bottom" src=@{StaticR images_religioso_04_png}>
            <div class="col-8">
                <form method="POST" action=@{CordecSignupR}>
                    <div class="form-group">
                        <label for="email">Email: 
                        <input id="email" class="form-control border-1 border-dark rounded-0" type="email" name="email" required>
                    <div class="form-group">
                        <label for="nome">Nome: 
                        <input id="nome" class="form-control border-1 border-dark rounded-0" type="text" name="nome">
                    <div class="form-group">
                        <label for="senha">Senha:
                        <input id="senha" class="form-control border-1 border-dark rounded-0" type="password" name="senha" required>
                    <button class="btn border-1 border-dark rounded-0">Cadastrar
            <div class="col-2">
                <img class="img-fluid bottom" src=@{StaticR images_maria_bonita_05_png}>
    |]