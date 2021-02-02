{-
Name: Timilsina, Sandesh
Net ID: stimilsina@unm.edu
-}

-- Note: you cannot put a = b, where b is some built in function. I.e. no myTakeWhile = takeWhile
-- Problem 1, myTakeWhile

myTakeWhile :: (a-> Bool ) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile func (a:ls) = if func a
                        then a:myTakeWhile func ls
                        else []

p1tests = [myTakeWhile (/= ' ') "This is practice." == "This"]

-- Problem 2, mySpan
mySpan :: (a->Bool) -> [a] -> ([a],[a])
mySpan func ls =
    let helpRest _ [] = []
        helpRest func (a:ls) = if func a
                                then helpRest func ls
                                else a:ls
    in (myTakeWhile func ls, helpRest func ls)

p2tests = [mySpan (/= ' ') "This is practice." == ("This"," is practice.")]


-- Problem 3, combinations3
combinations3 :: Ord a => [a] -> [[a]]
combinations3 ls = [a:b:c:[] | a <- ls, b <- ls, c <- ls, a < b, b < c]
p3tests = [combinations3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]



-- Problem 4, runLengthEncode
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode ls =
    let frequency = length (fst (mySpan (== head(ls)) ls))
        leftOver = snd (mySpan (== head(ls)) ls)
    in [(head ls, frequency)] ++ runLengthEncode leftOver
p4tests = [runLengthEncode [4,2,2,1,1,1,1,4,4,4,4] == [(4,1),(2,2),(1,4),(4,4)], runLengthEncode "foo" == [('f',1),('o',2)]]

-- Problem 5, runLengthDecode
runLengthDecode :: [(a, Int)] -> [a]
runLengthDecode [] = []
runLengthDecode ls = 
    let element = take (snd (head ls)) (repeat (fst (head ls)))
    in element ++ runLengthDecode (tail ls)
p5tests = [runLengthDecode [(4,1),(2,2),(1,4),(4,4)] == [4,2,2,1,1,1,1,4,4,4,4], (runLengthDecode $ runLengthEncode "foobar") == "foobar"]

-- Problem 6, splitText
splitText :: Ord a => (a -> Bool) -> [a] -> [[a]]
splitText _ [] = []
splitText func ls =
    let helpRest _ [] = []
        helpRest func (a:ls) = if func a
                                then helpRest func ls
                                else [myTakeWhile func ls] ++ helpRest func ls
    in [myTakeWhile func ls] ++ helpRest func ls
p6tests = [splitText (/= ' ') "This is practice." == ["This","is","practice."]]

-- Problem 7, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher ls1 ls2 word = [ls2 !! position | position <- (map func word)]
    where func  a = length (myTakeWhile (/= a) ls1)
p7tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this",encipher [1..10] (map (\x -> x*x) [1..10]) [10,9..1] == [100,81,64,49,36,25,16,9,4,1],encipher [10,9..0] [10,9..0] [0..10] == [0,1,2,3,4,5,6,7,8,9,10],encipher (['A','C'..'Z'] ++ ['B','D'..'Z']) [1..26] ['A'..'Z'] == [1,14,2,15,3,16,4,17,5,18,6,19,7,20,8,21,9,22,10,23,11,24,12,25,13,26]]

-- Problem 8, goldbach
goldbach :: Int -> [(Int, Int)]
goldbach number = [(a,b) | a <- primes number, b <- primes number,a<=b, a + b  == number]
    where
        factors num = [(a,b) | a <- [1..num], b <- [1..num], a*b == num]
        primes num = [p |p <-[1..num], (length (factors p) == 2)]
p8tests = [goldbach 6 == [(3,3)], goldbach 9 == [(2,7)]]


-- Problem 9, increasing
increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [a] = True
increasing (first:second:ls) = and [first <= second, increasing (first:ls)]
p9tests = [increasing "ABBD", not $ increasing [100,99..1]]


-- Problem 10, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select func listOne listTwo = [b | (a,b) <- zip listOne listTwo, func a]
p10tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]


-- Problem 11, combinations
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 0 ls = [[]]
combinations 1 ls = [[x] | x <- ls]
combinations n ls = 
    let comb element = combinations (n-1) (tail (filter (>= element) ls))
    in concat (map (\a -> [a:b | b <- comb a]) ls)

p11tests = [combinations 3 "ABCDE" == ["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]]




-- Note: Uncomment the pNtests and in tests below and in tests once you have given a definiton for problem 12

-- Problem 12, ComplexInteger, real, imaginary
data ComplexInteger = ComplexInteger { real :: Int, imaginary :: Int}

p12tests = [real (ComplexInteger 1 2) == 1, imaginary (ComplexInteger 2 3) == 3]



--Problem 13, Eq
instance Eq ComplexInteger
    where ComplexInteger a b == (ComplexInteger x y) = (a == x) && (b ==y)

p13tests = [(ComplexInteger 1 2) /= (ComplexInteger 3 4)]



-- Problem 14, Show
instance Show ComplexInteger where
    show (ComplexInteger real 0) = show real
    show (ComplexInteger 0 imaginary) = show imaginary ++ "i"
    show (ComplexInteger real imaginary) = show real ++ (if imaginary > 0 then "+" else "-") ++ show imaginary ++ "i"

p14tests = [(show $ ComplexInteger 1 2) == "1+2i", (show $ ComplexInteger 1 0) == "1", (show $ ComplexInteger 0 1) == "1i"]


-- Problem 15, Num
instance Num ComplexInteger where
    (ComplexInteger a1 b1) + (ComplexInteger a2 b2) = (ComplexInteger (a1 + a2) (b1 + b2))
    (ComplexInteger a1 b1) * (ComplexInteger a2 b2) = ComplexInteger (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)

p15tests = [(ComplexInteger 1 2) * (ComplexInteger 3 4) == (ComplexInteger (-5) 10)]

tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests] ++[p12tests,p13tests,p14tests,p15tests]
likelyCorrect = (and [and t | t <- tests], if length tests < 15 then "lacking ComplexInteger tests?" else "")