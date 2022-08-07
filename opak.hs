main = putStrLn "Wassup"

umocni x = x * x

vynasob x [] = []
vynasob x (h:t) = x * h : vynasob x t

fac 0 = 1
fac x = x * fac (x - 1)

vyskrtni x [] = []
vyskrtni x (h:t) = if x == h then vyskrtni x t 
    else h : vyskrtni x t

vyskrtninas x [] = []
vyskrtninas x (h:t) = if (rem h x) == 0  then vyskrtninas x t 
    else h : vyskrtninas x t

vynasobnas [] = 1
vynasobnas (h:t) = h * vynasobnas t


porovnej x [] = False
porovnej 0 (h:t) = False
porovnej x (h:t) = if (rem x h) == 0 then True else porovnej x t

neniprvocislo :: Int -> Bool
neniprvocislo x = porovnej x [2..floor (sqrt (fromIntegral x) + 1)]

vypisprvo 0 = []
vypisprvo 1 = []
vypisprvo 2 = []
vypisprvo 3 = [2]
vypisprvo 4 = [3,2]
vypisprvo x = if (neniprvocislo (x - 1)) then vypisprvo (x - 1)
                else (x - 1) : vypisprvo (x - 1)

cim 0 [] = []
cim x (h:t) = if (rem x h) == 0 then return h
        else cim x t

cimdelit 0 = [0]
cimdelit x = cim x (reverse (vypisprvo x)) 

tocislo x = head (cimdelit x)

vydel :: Int -> Int
vydel 0 = 0
vydel 4 = 2
vydel x = div x (tocislo x)

rozklad :: Int -> [Int]
rozklad 0 = []
rozklad 1 = []
rozklad 2 = [2]
rozklad 4 = [2,2]
rozklad x = if (neniprvocislo x) then tocislo x : rozklad (vydel x) else [x]

