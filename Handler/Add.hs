module Handler.Add where

import Import

getAddR :: Handler Html
getAddR = defaultLayout [whamlet|
        <form method=post action=@{InsertR}>
            <select name="userOffenses">
                <option value="first">first
                <option value="second">second
                <option value="third">third
            <input type=text name="reportUserId" placeholder="User ID">
            <input type=text name="reportDisplayName" placeholder="Display Name">
            <input type=text name="reportIpAddress" placeholder="IP Address">
            <input type=text name="email" placeholder="Email">
            <input type=text name="reporterId" placeholder="Reporter ID">
            <input type=text name="reportName" placeholder="Reporter Name">
            <input type=text name="staffMember" placeholder="Staff Member:">
            <input type=text name="correctionIssued" placeholder="Correction Issued:">
            <textarea name="incidentSummary" placeholder="Incident Summary">
            <textarea name="additionalActions" placeholder="Additional Actions">
            <textarea name="notes" placeholder="Notes">
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
            <input type=text name="reportUserId" placeholder="User ID" value=#{reportUserId}>
            <input type=text name="reportDisplayName" placeholder="Display Name" value=#{renderMaybe reportDisplayName}>
            <input type=text name="reportIpAddress" placeholder="IP Address" value=#{renderMaybe reportIpAddress}>
            <input type=text name="email" placeholder="Email" value=#{renderMaybe reportEmail}>
            <input type=text name="reporterId" placeholder="Reporter ID" value=#{renderMaybe reportReporterId}>
            <input type=text name="reportName" placeholder="Reporter Name" value=#{renderMaybe reportReporterName}>
            <input type=text name="staffMember" placeholder="Staff Member:">
            <input type=text name="correctionIssued" placeholder="Correction Issued:">
            <textarea name="incidentSummary" placeholder="Incident Summary">
            <textarea name="additionalActions" placeholder="Additional Actions">
            <textarea name="notes" placeholder="Notes">
            <input type=submit value=Submit>
    |]