module Handler.Add where

import Import

getAddR :: Handler Html
getAddR = defaultLayout [whamlet|
        <form method=post action=@{InsertR}>
            <table>
                <tr>
                    <td><label for="userOffenses">Offenses: </label>
                    <td>^{renderReportTypeOption}
                <tr>
                    <td><label for="reportUserId">User ID: </label>
                    <td><input type=text name="reportUserId" placeholder="User ID">
                <tr>
                    <td><label for="reportDisplayName">Display Name: </label>
                    <td><input type=text name="reportDisplayName" placeholder="Display Name">
                <tr>
                    <td><label for="reportIpAddress">IP Address: </label>
                    <td><input type=text name="reportIpAddress" placeholder="IP Address">
                <tr>
                    <td><label for="email">Email: </label>
                    <td><input type=text name="email" placeholder="Email">
                <tr>
                    <td><label for="reporterId">Reporter User ID: </label>
                    <td><input type=text name="reporterId" placeholder="Reporter ID">
                <tr>
                    <td><label for="reportName">Reporter Name: </label>
                    <td><input type=text name="reportName" placeholder="Reporter Name">
                <tr>
                    <td><label for="staffMember">Staff Member (you): </label>
                    <td><input type=text name="staffMember" placeholder="Staff Member:">
                <tr>
                    <td><label for="correctionIssued">Correction Issued: </label>
                    <td>^{renderCorrectionTypeOption}
                <tr>
                    <td><label for="incidentSummary">Incident Summary: </label>
                    <td>
                        <textarea rows=5 cols=50 name="incidentSummary" placeholder="Incident Summary">
                <tr>
                    <td><label for="additionalActions">Additional Actions: </label>
                    <td>
                        <textarea rows=5 cols=50 name="additionalActions" placeholder="Additional Actions">
                <tr>
                    <td><label for="notes">Notes: </label>
                    <td>
                        <textarea rows=5 cols=50 name="notes" placeholder="Notes">
                <tr>
                    <td><input type=submit value=Submit>
    |]

postAddR :: Handler Html
postAddR = do
    reportUserId       <- runInputPost $ iopt textField "reportUserId"
    reportDisplayName  <- runInputPost $ iopt textField "reportDisplayName"
    reportIpAddress    <- runInputPost $ iopt textField "reportIpAddress"
    reportEmail        <- runInputPost $ iopt textField "email"
    reportReporterId   <- runInputPost $ iopt textField "reporterId"
    reportReporterName <- runInputPost $ iopt textField "reportName"
    defaultLayout [whamlet|
        <form method=post action=@{InsertR}>
            <table>
                <tr>
                    <td><label for="userOffenses"></label>
                    <td>^{renderReportTypeOption}
                <tr>
                    <td><label for="reportUserId">User ID: </label>
                    <td><input type=text name="reportUserId" placeholder="User ID" value=#{renderMaybeText reportUserId}>
                <tr>
                    <td><label for="reportDisplayName">Display Name: </label>
                    <td><input type=text name="reportDisplayName" placeholder="Display Name" value=#{renderMaybeText reportDisplayName}>
                <tr>
                    <td><label for="reportIpAddress">IP Address: </label>
                    <td><input type=text name="reportIpAddress" placeholder="IP Address" value=#{renderMaybeText reportIpAddress}>
                <tr>
                    <td><label for="email">Email: </label>
                    <td><input type=text name="email" placeholder="Email" value=#{renderMaybeText reportEmail}>
                <tr>
                    <td><label for="reporterId">Reporter User ID: </label>
                    <td><input type=text name="reporterId" placeholder="Reporter ID" value=#{renderMaybeText reportReporterId}>
                <tr>
                    <td><label for="reportName">Reporter Name: </label>
                    <td><input type=text name="reportName" placeholder="Reporter Name" value=#{renderMaybeText reportReporterName}>
                <tr>
                    <td><label for="staffMember">Staff Member (you): </label>
                    <td><input type=text name="staffMember" placeholder="Staff Member:">
                <tr>
                    <td><label for="correctionIssued">Correction Issued: </label>
                    <td><input type=text name="correctionIssued" placeholder="Correction Issued:">
                <tr>
                    <td><label for="incidentSummary">Incident Summary: </label>
                    <td>
                        <textarea rows=5 cols=50 name="incidentSummary" placeholder="Incident Summary">            
                <tr>
                    <td><label for="additionalActions">Additional Actions: </label>
                    <td>
                        <textarea rows=5 cols=50 name="additionalActions" placeholder="Additional Actions">
                <tr>
                    <td><label for="notes">Notes: </label>
                    <td>
                        <textarea rows=5 cols=50 name="notes" placeholder="Notes">
                <tr>
                    <td><input type=submit value=Submit>
    |]
