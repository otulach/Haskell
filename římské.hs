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

hodnota2 1 = 'I'
hodnota2 5 = 'V'
hodnota2 10 = 'X'
hodnota2 50 = 'L'
hodnota2 100 = 'C'
hodnota2 500 = 'D'
hodnota2 1000 = 'M'

zřímských :: String -> Int
zřímských "" = 0
zřímských [h] = hodnota h
zřímských [h, h1] = if hodnota h < hodnota h1 then (hodnota h1 - hodnota h) else hodnota h + hodnota h1
zřímských (h : h1 : h2 : t) = if hodnota h == hodnota h1 && hodnota h2 > hodnota h1 then (hodnota h2 - (hodnota h + hodnota h1)) + zřímských t
                                else if hodnota h == hodnota h1 then (hodnota h) + zřímských (h1 : h2 : t)
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
    TestCase "13" (nařímské 13) "XIII",
    TestCase "8" (nařímské 8) "VIII",
    TestCase "999" (nařímské 999) "CMXCIX",
    TestCase "end of tests" "ok" "ok"
    ]

testoba = ověř $ map testuj [1..3000] where
    testuj :: Int -> Testable
    testuj x = TestCase "" x a where
        r = nařímské x
        a :: Int
        a = zřímských r

nařímské :: Int -> String
nařímské 0 = []
nařímské a = if a >= 1000 then hodnota2 1000 : nařímské (a - 1000) else
                if a >= 900 then hodnota2 100 : hodnota2 1000 : nařímské (a - 900) else
                if a >= 800 then hodnota2 100 : hodnota2 100 : hodnota2 1000 : nařímské (a - 800) else    
            if a >= 500 then hodnota2 500 : nařímské (a - 500) else
                if a >= 450 then hodnota2 50 : hodnota2 500 : nařímské (a - 450) else
                if a >= 400 then hodnota2 100 : hodnota2 500 : nařímské (a - 400) else
            if a >= 100 then hodnota2 100 : nařímské (a - 100) else
                if a >= 90 then hodnota2 10 : hodnota2 100 :  nařímské (a - 90) else
                if a >= 80 then hodnota2 10 : hodnota2 10 : hodnota2 100 : nařímské (a - 80) else
            if a >= 50 then hodnota2 50 : nařímské (a - 50) else
                if a >= 45 then hodnota2 5 : hodnota2 50 :  nařímské (a - 45) else
                if a >= 40 then hodnota2 10 : hodnota2 50 : nařímské (a - 40) else
            if a >= 10 then hodnota2 10 : nařímské (a - 10) else
                if a >= 9 then hodnota2 1 : hodnota2 10 :  nařímské (a - 9) else
            if a >= 5 then hodnota2 5 : nařímské (a - 5) else
                if a >= 4 then hodnota2 1 : hodnota2 5 :  nařímské (a - 4) else
            hodnota2 1 : nařímské (a - 1)