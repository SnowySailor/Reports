module Handler.Edit where

import Import
import qualified Database.Persist.Sql as DB
--import qualified Data.Text (empty)

getEditR :: ReportId -> Handler Html
getEditR reportId = do
    [Entity _ report] <- runDB $ selectList [ReportId ==. reportId] []
    defaultLayout [whamlet|
            <form method=post action=@{EditR reportId}>
                <input type=text name=staffMember value=#{reportStaffMember report}>
                <input type=text name=correctionIssued value=#{renderMaybeText $ reportCorrectionIssued report}>
                <textarea name=incidentSummary>#{renderMaybeText $ reportIncidentSummary report}
                <textarea name=additionalActions>#{renderMaybeText $ reportAdditionalActions report}
                <textarea name=notes>#{renderMaybeText $ reportNotes report}
                <input type=submit value=Edit>
    |]

postEditR :: ReportId -> Handler ()
postEditR reportId = do
    [Entity _ report] <- runDB $ selectList [ReportId ==. reportId] []
    staffName         <- runInputPost $ ireq textField "staffMember"
    correctionIssued  <- runInputPost $ iopt textField "correctionIssued"
    summary           <- runInputPost $ iopt textField "incidentSummary"
    additionalActions <- runInputPost $ iopt textField "additionalActions"
    notes             <- runInputPost $ iopt textField "notes"
    time              <- liftIO $ getCurrentTime
    let offenses      = reportUserOffenses report
        userid        = reportUserId report
        display       = reportDisplayName report
        ip            = reportIpAddress report
        email         = reportEmail report
        reporterid    = reportReporterId report
        reportername  = reportReporterName report
        closed        = False
        reportChain   = case reportReportChain report of; Just val -> Just val; Nothing -> Just $ fromIntegral $ DB.fromSqlKey reportId;
        newReport     = Report time offenses userid display ip email reporterid reportername staffName correctionIssued summary additionalActions notes closed reportChain
    _ <- runDB $ insert newReport
    _ <- runDB $ update reportId [ReportClosed =. True]
    setMessage "Done"
    redirectUltDest HomeR