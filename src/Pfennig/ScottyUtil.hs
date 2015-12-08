module ScottyUtil where

import qualified Data.Text.Lazy   as TL
import           Web.Scotty       (Parsable, parseParam)
import           Web.Scotty.Trans (ActionT, ScottyError, params)

-- | retrieve parameter from request and fail in either
paramSafe :: (Monad m, ScottyError e, Parsable a) => TL.Text -> ActionT e m (Either TL.Text a)
paramSafe k = do
  ps <- params
  return $ case lookup k ps of
    Nothing -> Left $ TL.pack "param not found"
    Just a -> parseParam a
