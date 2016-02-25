module Handler.Insert where

import Import
import Data.Text.Read as R (decimal)

postInsertR :: Handler ()
postInsertR = do
    time <- liftIO $ getCurrentTime
    staffName         <- runInputPost $ ireq textField "staffMember"
    correctionIssued  <- runInputPost $ iopt textField "correctionIssued"
    summary           <- runInputPost $ iopt textField "incidentSummary"
    additionalActions <- runInputPost $ iopt textField "additionalActions"
    notes             <- runInputPost $ iopt textField "notes"
    offense           <- runInputPost $ ireq textField "userOffenses"
    reportUserId      <- runInputPost $ ireq textField "reportUserId"
    reportDisplayName <- runInputPost $ iopt textField "reportDisplayName"
    reportIpAddress   <- runInputPost $ ireq textField "reportIpAddress"
    email             <- runInputPost $ iopt textField "email"
    reporterId        <- runInputPost $ iopt textField "reporterId"
    reportName        <- runInputPost $ iopt textField "reportName"
    let reportedUserId = case decimal reportUserId of; Right (a,_) -> a; _ -> -1
        --reporterUserId = case reporterId of; Just val -> Just ((read $ T.unpack val) :: Int); _ -> Nothing; 
        reporterUserId = case reporterId of
                            Just a -> case R.decimal a of
                                        Right (val,_) -> Just val
                                        _ -> Nothing
                            _ -> Nothing
        closed         = False
    _ <- runDB $ insert (Report time offense reportedUserId reportDisplayName reportIpAddress email reporterUserId reportName staffName correctionIssued summary additionalActions notes closed)

    setMessage "Inserted"
    redirect HomeR
