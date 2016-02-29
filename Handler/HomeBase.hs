module Handler.HomeBase where

import Import

getHomeBaseR :: Handler ()
getHomeBaseR = do
	login <- lookupSession "userId"
	case login of
		Nothing -> redirectUltDest LoginUserR
		Just _  -> redirectUltDest HomeR
