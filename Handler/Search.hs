module Handler.Search where

import Import
import Prelude (read)
import Data.Text as T

postSearchR :: Handler Html
postSearchR = do
    searchType <- runInputPost $ ireq textField "searchType"
    search     <- runInputPost $ ireq textField "search"
    case searchType of
        "IP" -> do 
            reports <- runDB $ selectList [ReportIpAddress ==. search] [Desc ReportId]
            renderReports reports

        "UserId" -> do 
            reports <- runDB $ selectList [ReportUserId ==. (read (T.unpack search) :: Int)] [Desc ReportId]
            renderReports reports

        "Staff" -> do
            reports <- runDB $ selectList [ReportStaffMember `like` search] [Desc ReportId]
            renderReports reports
        _ -> do
            defaultLayout [whamlet|Unsupported search type.|]


renderReports :: (MonoFoldable (t (Entity Report)), Foldable t) => t (Entity Report) -> HandlerT App IO Html
renderReports reports = defaultLayout $ do
                    setTitle "Search"
                    $(widgetFile "search")