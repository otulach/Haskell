{-# LANGUAGE ExistentialQuantification #-}

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

data Testable = forall a . (Eq a, Show a) => TestCase String a a
instance Show Testable
  where
  showsPrec p (TestCase a b c) = showsPrec p (a, b, c)

ověř :: [Testable] -> [Testable]
ověř [] = []
ověř (TestCase a b c:t) = if b == c then ověř t 
                else TestCase a b c : ověř t    

test = ověř [
    TestCase "10" (zřímských "X") 10,
    TestCase "1984" (zřímských "MCMLXXXIV") 1984,
    TestCase "end of tests" "ok" "ok"
    ]