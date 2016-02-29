module Handler.MarkDone where

import Import

postMarkDoneR :: ReportId -> Handler ()
postMarkDoneR reportId = do
    _ <- runDB $ update reportId [ReportClosed =. True]
    redirect HomeR

getMarkDoneR :: ReportId -> Handler ()
getMarkDoneR reportId = do
    _ <- runDB $ update reportId [ReportClosed =. True]
    redirect HomeR