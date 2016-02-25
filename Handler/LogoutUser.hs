module Handler.LogoutUser where

import Import

postLogoutUserR :: Handler ()
postLogoutUserR = logout

getLogoutUserR :: Handler ()
getLogoutUserR = logout

logout :: (MonadHandler m, RedirectUrl (HandlerSite m) (Route App)) => m b
logout = do
    deleteSession "userName"
    deleteSession "userId"
    deleteSession "fullName"
    deleteSession "userGroup"
    redirectUltDest LoginUserR