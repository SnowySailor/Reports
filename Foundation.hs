module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

--Custom
import qualified Database.Persist.Sql as DB
import Text.Lucius (luciusFile)
import qualified Data.Text as T
import qualified Text.Blaze (ToMarkup)
import qualified Text.Blaze.Internal (MarkupM)
import qualified Database.MySQL.Simple as M
import qualified Data.Time.Format as F
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryParams


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            returnCss
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized LoginUserR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized HomeBaseR _ = return Authorized
    isAuthorized (MarkDoneR _) _ = isUserAuthorized
    -- Default to Authorized for now.
    isAuthorized _ _ = isUserAuthorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> Just <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

isUserAuthorized :: MonadHandler m => m AuthResult
isUserAuthorized = do
    session <- lookupSession "userId"
    return $ case session of
        Nothing -> Unauthorized "Must be an admin. Please login."
        Just _ -> Authorized


--Custom helpers
renderMaybeText :: Maybe Text -> Text
renderMaybeText may = case may of
                    Just val -> val
                    Nothing -> T.empty
renderMaybe :: Show a => Maybe a -> Text
renderMaybe may = case may of
                    Just val -> T.pack $ show val
                    Nothing ->T.empty

secretMessage :: String
secretMessage = "there were so many cheeses he died"

renderReportTable :: (Text.Blaze.ToMarkup a, MonoFoldable (t (Entity Report)), Foldable t) => t (Entity Report) -> (Route App -> [t1] -> a) -> Text.Blaze.Internal.MarkupM ()
renderReportTable reports = [hamlet|
    $if null reports
        Nothing to see here
    $else
        <table .table .table-boardered >
            <tr>
                <td .tableHeader >ID
                <td .tableHeader >Time
                <td .tableHeader >Offenses
                <td .tableHeader >UserId
                <td .tableHeader >Display Name
                <td .tableHeader >IP
                <td .tableHeader >Staff Member
                <td .tableHeader >Correction Issued
            $forall Entity reportId report <- reports
                <tr>
                    <td>#{DB.fromSqlKey reportId}
                    <td>#{F.formatTime F.defaultTimeLocale "%F" $ reportTime report}
                    <td>#{reportUserOffenses report}
                    <td>#{reportUserId report}
                    <td>#{renderMaybeText $ reportDisplayName report}
                    <td>#{reportIpAddress report}
                    <td>#{reportStaffMember report}
                    <td>#{renderMaybeText $ reportCorrectionIssued report}
                    <td .edit ><a href=@{EditR reportId}><input type=button value=Edit>
                    $if not $ reportClosed report
                        <td .mark-done ><a href=@{MarkDoneR reportId}><input type=button value="Mark Done">
                    <td .view ><a href=@{ViewR reportId}><input type=button value="View">
|]

searchBar :: Text.Blaze.ToMarkup a => (Route App -> [t] -> a) -> Text.Blaze.Internal.MarkupM ()
searchBar = [hamlet|
    <form method=post action=@{SearchR}>
        <select name="searchType">
            <option value="IP">IP
            <option value="UserId">User Id
        <input type=text name="search" placeholder="Search:">
        <input type=submit value="Search">
|]

returnCss :: MonadWidget m => m ()
returnCss = toWidget $(luciusFile "templates/overArch.lucius")

goHome :: Text.Blaze.ToMarkup a => (Route App -> [t] -> a) -> Text.Blaze.Internal.MarkupM ()
goHome = [hamlet|
    <a .home href=@{HomeBaseR}>Go Home
|]

renderReport :: (Text.Blaze.ToMarkup a, MonoFoldable (t (Entity Report)), Foldable t) => t (Entity Report) -> (Route App -> [t1] -> a) -> Text.Blaze.Internal.MarkupM ()
renderReport reports = [hamlet|
    $if null reports
        Nothing to see here
    $else  
        $forall Entity reportId report <- reports
            <table .table .table-boardered .singleReportTable>
                <tr>
                    <td .tableHeader>ID
                    <td>#{DB.fromSqlKey reportId}
                <tr>
                    <td .tableHeader >Time
                    <td>#{F.formatTime F.defaultTimeLocale "%F" $ reportTime report}
                <tr>
                    <td .tableHeader >Offenses
                    <td>#{reportUserOffenses report}
                <tr>
                    <td .tableHeader >UserId
                    <td>#{reportUserId report}
                <tr>
                    <td .tableHeader >Display Name
                    <td>#{renderMaybeText $ reportDisplayName report}
                <tr>
                    <td .tableHeader >IP
                    <td>#{reportIpAddress report}
                <tr>
                    <td .tableHeader >Email
                    <td>#{renderMaybeText $ reportEmail report}
                <tr>
                    <td .tableHeader >Reporter ID
                    <td>#{renderMaybe $ reportReporterId report}
                <tr>
                    <td .tableHeader >Reporter Name
                    <td>#{renderMaybeText $ reportReporterName report}
                <tr>
                    <td .tableHeader >Staff Member
                    <td>#{reportStaffMember report}
                <tr>
                    <td .tableHeader >Correction Issued
                    <td>#{renderMaybeText $ reportCorrectionIssued report}
                <tr>
                    <td .tableHeader >Summary
                    <td>#{renderMaybeText $ reportIncidentSummary report}
                <tr>
                    <td .tableHeader >Actions
                    <td>#{renderMaybeText $ reportAdditionalActions report}
                <tr>
                    <td .tableHeader >Notes
                    <td>#{renderMaybeText $ reportNotes report}
                <tr>
                    <td>
                        <a href=@{EditR reportId}><input type=button value="Edit">
                $if not $ reportClosed report
                    <td>
                        <a href=@{MarkDoneR reportId}><input type=button value="Mark Done">
|]

pageSelection :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Int64 -> WidgetT App m ()
pageSelection page = [whamlet|
            $if page > 1
                <a href=@{PageR (page - 1)} >Previous
            <a href=@{PageR (page + 1)} >Next
        |]

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

credsMysql :: M.ConnectInfo
credsMysql = M.defaultConnectInfo 
    { M.connectHost     = "0.0.0.0"
    , M.connectUser     = "root"
    , M.connectPassword = "rooty"
    , M.connectDatabase = "reports"
    , M.connectPort     = 3306
    , M.connectPath     = ""
    }

getReportTypes :: [Text]
getReportTypes = map T.pack ["Administrative Abuse", "Drug Use", "Inappropriate Character", "Mini Modding", "Name Violation", "Spam / Unnessesary OOC", "[NSFW] Gore & Violence", "[NSFW] Nudity or Pornography/Sexual Material", "Attacks Individual or Group", "Copyright Infringement"]

renderReportTypeOption :: t -> Text.Blaze.Internal.MarkupM ()
renderReportTypeOption = [hamlet|
    <select name="userOffenses">
        $forall option <- getReportTypes
            <option value=#{option}>#{option}
    |]

