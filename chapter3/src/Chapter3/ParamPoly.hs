module Chapter3.ParamPoly where

maybeString :: Maybe t -> [Char]
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

data Client i = GovOrg      { clientId :: i, clientName :: String }
              | Company     { clientId :: i
                             , clientName :: String
                             , person :: Person
                             , duty :: String }
              | Individual  { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName :: String }
            deriving Show

data Triple a b c = Triple a b c
