module Handler.View where

import Import
import qualified Database.Persist.Sql as DB

getViewR :: ReportId -> Handler Html
getViewR reportId = do
    report <- runDB $ selectList [ReportId ==. reportId] []
    let [Entity _ singleReport] = report
        reportChainId = reportReportChain $ singleReport
    extraIds <- runDB $ selectList [ReportReportChain ==. reportChainId, ReportReportChain !=. Nothing, ReportId !=. reportId] []
    defaultLayout [whamlet|
        ^{renderReport report}
        <br>
        $if not $ null extraIds
            <p>Report versions that came before or after this
            <table>
                <tr>
                    $forall Entity rId _ <- extraIds
                        <td .reportChainData ><a href=@{ViewR rId}>#{DB.fromSqlKey rId}
    |]
 