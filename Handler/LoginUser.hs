module Handler.LoginUser where

import Import
import qualified Database.MySQL.Simple as M
import qualified Crypto.Hash.MD5 as H (hash)
import qualified Data.Text.Encoding as E (encodeUtf8)
import qualified Data.ByteString as B (ByteString, concat)
import qualified Data.ByteString.Base16 as BB (encode)

getLoginUserR :: Handler Html
getLoginUserR = do
    login <- lookupSession "userId"
    msg <- getMessage
    case login of
        Nothing -> defaultLayout [whamlet|
                <p>You are not logged in.
                $maybe mmsg <- msg
                    #{mmsg}
                <form method=post action=@{LoginUserR}>
                    <input type=text name=user placeholder="Username:">
                    <input type=password name=password placeholder="Password:">
                    <input type=submit value="Login">
                |]
        Just login -> do
            sess <- getSession
            defaultLayout [whamlet|
                        You are logged in. 
                        #{show sess}
                        <form method=post action=@{LogoutUserR}>
                            <input type=submit value="Logout">
                    |]

postLoginUserR :: Handler ()
postLoginUserR = do
    name <- runInputPost $ ireq textField "user"
    password <- runInputPost $ ireq passwordField "password"
    creds <- liftIO $ getCreds [name]

    if null creds then do
        setMessage "Invalid user."
        redirectUltDest LoginUserR
        else do
            let singleUser:_ = creds
                passSalt     = fromString (salt singleUser)
                passHash     = fromString $ pack (realHash singleUser)
                inputPass    = E.encodeUtf8 password
                inputHash    = BB.encode . H.hash . BB.encode . B.concat $ map H.hash [inputPass, passSalt]
            if passHash == inputHash then do
                loginData <- liftIO $ getUser [name]
                let user:_ = loginData
                setSession "userName"  $ pack (userName user)
                setSession "fullName"  $ pack (fullName user)
                setSession "userId"    $ pack $ show (userId user)
                setSession "userGroup" $ pack $ show (userGroup user)
                setMessage "Logged"
                redirectUltDest HomeR
                else do
                    setMessage "Incorrect login. Please try again."
                    redirectUltDest LoginUserR

