module Handler.Edit where

import Import
import qualified Database.Persist.Sql as DB
--import qualified Data.Text (empty)

getEditR :: ReportId -> Handler Html
getEditR reportId = do
    [Entity _ report] <- runDB $ selectList [ReportId ==. reportId] []
    defaultLayout [whamlet|
            <form method=post action=@{EditR reportId}>
                <table>
                    <tr>
                        <td><label for="staffMember">Staff member:
                        <td><input type=text required name=staffMember value=#{reportStaffMember report}>
                    <tr>
                        <td><label for="correctionIssued">Correction Issued:
                        <td><input type=text name=correctionIssued value=#{renderMaybeText $ reportCorrectionIssued report}>
                    <tr>
                        <td><label for="incidentSummary">Incident Summary:
                        <td>
                            <textarea rows=5 cols=50 name=incidentSummary>#{renderMaybeText $ reportIncidentSummary report}
                    <tr>
                        <td><label for="additionalActions">Additional Actions:
                        <td>
                            <textarea rows=5 cols=50 name=additionalActions>#{renderMaybeText $ reportAdditionalActions report}
                    <tr>
                        <td><label for="notes">Notes:
                        <td>
                            <textarea rows=5 cols=50 name=notes>#{renderMaybeText $ reportNotes report}
                    <tr>
                        <td><input type=submit value=Edit>
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