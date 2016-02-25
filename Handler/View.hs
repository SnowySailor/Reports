module Handler.View where

import Import

getViewR :: ReportId -> Handler Html
getViewR reportId = do
    report <- runDB $ selectList [ReportId ==. reportId] []
    defaultLayout [whamlet|
        ^{renderReport report}
    |]
