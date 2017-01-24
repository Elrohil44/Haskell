module List
 (safeHead
 ,safeTail
 ) where


safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (h:t) = Just h

safeTail [] = Nothing
safeTail (h:t) = Just t