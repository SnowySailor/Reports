module Handler.Home where

import Import
import qualified Database.Persist.Sql as DB

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    auth <- lookupSession "userId"
    name <- lookupSession "fullName"
    let page = 1
    case auth of
        Nothing -> do
            redirect LoginUserR
        Just _  -> do
            sess <- getSession
            reports <- getReports
            defaultLayout $ do
                    setTitle "Reports"
                    $(widgetFile "report-list")

postHomeR :: Handler ()
postHomeR = do
    name <- runInputPost $ ireq textField "name"
    setSession "name" name
    redirectUltDest HomeR

postRemoveNameR :: Handler ()
postRemoveNameR = do
    deleteSession "name"
    redirectUltDest HomeR


getPageR :: Int64 -> Handler Html
getPageR page = do
    [Entity reid _] <- runDB $ selectList ([] :: [Filter Report]) [Desc ReportId, LimitTo 1]
    let limit = (page - 1) * 30
        lastEntry = DB.fromSqlKey reid
        selectFromRaw = lastEntry - limit
    --calculateHidden <- runDB $ selectList [ReportClosed ==. True, ReportId >=. (DB.toSqlKey selectFromRaw)] []
    [SingleReturn calculateHidden] <- liftIO $ getCalculateHidden [selectFromRaw]
    --Needs reworked to actually work right.
    let selectFrom = DB.toSqlKey $ (fromIntegral selectFromRaw) - (fromIntegral $ calculateHidden)
    sess <- getSession
    name <- lookupSession "fullName"
    reports <- runDB $ selectList [ReportClosed ==. False, ReportId <=. selectFrom] [Desc ReportId, LimitTo 30]
    defaultLayout $ do
        setTitle "Reports"
        $(widgetFile "report-list")



getAllReports :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => HandlerT site IO [Entity Report]
getAllReports = runDB $ selectList ([] :: [Filter Report]) [Desc ReportId, LimitTo 30]

getReports:: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => HandlerT site IO [Entity Report]
getReports = runDB $ selectList [ReportClosed ==. False] [Desc ReportId, LimitTo 30]


