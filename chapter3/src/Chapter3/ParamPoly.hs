module Chapter3.ParamPoly where

maybeString :: Maybe t -> [Char]
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"
