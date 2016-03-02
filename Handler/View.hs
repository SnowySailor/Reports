module Handler.View where

import Import

getViewR :: ReportId -> Handler Html
getViewR reportId = do
    report <- runDB $ selectList [ReportId ==. reportId] []
    let [Entity _ singleReport] = report
        reportChainId = reportReportChain $ singleReport
    extraIds <- runDB $ selectList [ReportReportChain ==. (reportChainId)] []
    defaultLayout [whamlet|
        ^{renderReport report}
        <br>
        #{show extraIds}
    |]
 