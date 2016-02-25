module Handler.LoginUser where

import Import
import qualified Database.MySQL.Simple as M
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryParams
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
    mconn <- liftIO $ M.connect credsMysql

    creds <- liftIO $ getCreds [name] mconn

    if null creds then do
        setMessage "Invalid user."
        redirectUltDest LoginUserR
        else do
            let singleUser:_ = creds
                passSalt = fromString (salt singleUser)
                passHash = fromString $ pack (realHash singleUser)
                inputPass = E.encodeUtf8 password
                inputHash = BB.encode . H.hash . BB.encode . B.concat $ map H.hash [inputPass, passSalt]
            if passHash == inputHash then do
                loginData <- liftIO $ getUser [name] mconn
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

userQuery :: M.Query
userQuery = "select user_name, user_id, full_name, user_group_id from users where user_group_id in (1,6,7) and user_name = ?"

credsQuery :: M.Query
credsQuery = "select password, password_salt from users where user_name = ?"

getCreds :: QueryParams q => q -> M.Connection -> IO [Credentials]
getCreds qs connect = do
    creds <- M.query connect credsQuery qs
    return creds

getUser :: QueryParams q => q -> M.Connection -> IO [Staff]
getUser qs connect = do
    users <- M.query connect userQuery qs
    return users

data Staff = Staff {userName :: String, fullName :: String, userId :: Int, userGroup :: Int} deriving Show
instance QueryResults Staff where
    convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = Staff {userName = a, userId = b, fullName = c, userGroup = d}
        where a = convert fa va
              b = convert fb vb
              c = convert fc vc
              d = convert fd vd

data Credentials = Credentials {realHash :: String, salt :: String} deriving Show
instance QueryResults Credentials where
    convertResults [fa, fb] [va,vb] = Credentials {realHash = a, salt = b}
        where a = convert fa va
              b = convert fb vb