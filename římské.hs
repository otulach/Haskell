hodnota :: Char -> Int
hodnota 'I' = 1
hodnota 'V' = 5
hodnota 'X' = 10
hodnota 'L' = 50
hodnota 'C' = 100
hodnota 'D' = 500
hodnota 'M' = 1000
hodnota _ = 0

zřímských :: String -> Int
zřímských "" = 0
zřímských [h] = hodnota h
zřímských [h, h1] = if hodnota h < hodnota h1 then (hodnota h1 - hodnota h) else hodnota h + hodnota h1
zřímských (h : h1 : h2 : t) = if hodnota h == hodnota h1 then (hodnota h2 - (hodnota h + hodnota h1)) + zřímských t
                                else if hodnota h < hodnota h1 then (hodnota h1 - hodnota h) + zřímských (h2 : t)
                                else (hodnota h) + zřímských (h1 : h2 : t) 