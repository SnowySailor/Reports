module Handler.Add where

import Import

getAddR :: Handler Html
getAddR = defaultLayout [whamlet|
        <form method=post action=@{InsertR}>
            <select name="userOffenses">
                <option value="first">first
                <option value="second">second
                <option value="third">third
            <br>
            <input type=text name="reportUserId" placeholder="User ID"><br>
            <input type=text name="reportDisplayName" placeholder="Display Name"><br>
            <input type=text name="reportIpAddress" placeholder="IP Address"><br>
            <input type=text name="email" placeholder="Email"><br>
            <input type=text name="reporterId" placeholder="Reporter ID"><br>
            <input type=text name="reportName" placeholder="Reporter Name"><br>
            <input type=text name="staffMember" placeholder="Staff Member:"><br>
            <input type=text name="correctionIssued" placeholder="Correction Issued:"><br>
            <textarea name="incidentSummary" placeholder="Incident Summary">
            <br>
            <textarea name="additionalActions" placeholder="Additional Actions">
            <br>
            <textarea name="notes" placeholder="Notes">
            <br>
            <input type=submit value=Submit>
    |]

postAddR :: Handler Html
postAddR = do
    reportUserId       <- runInputPost $ ireq textField "reportUserId"
    reportDisplayName  <- runInputPost $ iopt textField "reportDisplayName"
    reportIpAddress    <- runInputPost $ iopt textField "reportIpAddress"
    reportEmail        <- runInputPost $ iopt textField "email"
    reportReporterId   <- runInputPost $ iopt textField "reporterId"
    reportReporterName <- runInputPost $ iopt textField "reportName"
    defaultLayout [whamlet|
        <form method=post action=@{InsertR}>
            <select name="userOffenses">
                <option value="first">first
                <option value="second">second
                <option value="third">third
            <br>
            <input type=text name="reportUserId" placeholder="User ID" value=#{reportUserId}><br>
            <input type=text name="reportDisplayName" placeholder="Display Name" value=#{renderMaybe reportDisplayName}><br>
            <input type=text name="reportIpAddress" placeholder="IP Address" value=#{renderMaybe reportIpAddress}><br>
            <input type=text name="email" placeholder="Email" value=#{renderMaybe reportEmail}><br>
            <input type=text name="reporterId" placeholder="Reporter ID" value=#{renderMaybe reportReporterId}><br>
            <input type=text name="reportName" placeholder="Reporter Name" value=#{renderMaybe reportReporterName}><br>
            <input type=text name="staffMember" placeholder="Staff Member:"><br>
            <input type=text name="correctionIssued" placeholder="Correction Issued:"><br>
            <textarea name="incidentSummary" placeholder="Incident Summary">
            <br>
            <textarea name="additionalActions" placeholder="Additional Actions">
            <br>
            <textarea name="notes" placeholder="Notes">
            <br>
            <input type=submit value=Submit>
    |]