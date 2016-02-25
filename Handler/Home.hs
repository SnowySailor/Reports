module Handler.Home where

import Import

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
    case auth of
        Nothing -> do
            redirect LoginUserR
        Just _ -> do
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

getAllReports :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => HandlerT site IO [Entity Report]
getAllReports = runDB $ selectList ([] :: [Filter Report]) [Desc ReportTime]

getReports:: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => HandlerT site IO [Entity Report]
getReports = runDB $ selectList [ReportClosed ==. False] [Desc ReportTime]


