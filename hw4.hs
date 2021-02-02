{-
Name: Timilsina, Sandesh
Net ID: stimilsina@unm.edu
-}
-- Problem 1, stutter
stutter :: [a] -> [a]
stutter [] = []
stutter (x:ls) = x:x:stutter ls
 
p1tests = [(stutter "Hello World") == "HHeelllloo  WWoorrlldd", (stutter [1,2,3]) == [1,1,2,2,3,3]]
--comments


-- Problem 2, compress
compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:ls) = if a == b then compress (a : ls) else a:(compress (b:ls))
p2tests = [compress "HHeelllloo WWoorrlldd" == "Helo World",  compress [1,2,2,3,3,3] == [1,2,3]]


-- Problem 3, findIndices
findIndices :: (a -> Bool) -> [a] -> [Int]                                                    
findIndices cond_given ls = helper cond_given ls 0
    where 
        helper _ [] _ = [] 
        helper cond_given (a:ls) i = if (cond_given(a)) then (i) : (helper cond_given ls (i+1)) else helper cond_given ls (i+1)

p3tests = [findIndices (< 'a') "AbCdef" == [0,2],findIndices (== 0) [1,2,0,3,0] == [2,4]]


-- Problem 3.5, intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (a:ls1) ls2 = if elem a ls2 then a:(intersect ls1 ls2) else intersect ls1 ls2
p35tests = [intersect "abc" "cat" == "ac", intersect [1,2,3] [8] == [], intersect [3,2,1] [1,2,3] == [3,2,1]]

-- Problem 4, isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:ls1) (b:ls2) = if a ==b then True && isPrefixOf ls1 ls2 else False
p4tests = ["foo" `isPrefixOf` "foobar", not $ isPrefixOf [1,2,3] [4,5,6]]

-- Problem 5, isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (ls1) (ls2) = if last ls1 == last ls2 then True && isSuffixOf (init ls1) (init ls2) else False
p5tests = ["bar" `isSuffixOf` "foobar", not $ isSuffixOf [1,2,3] [4,5,6]]


-- Problem 6, dot
dot :: [Int] -> [Int] -> Int
dot [] [] = 0
dot (a:ls1) (b:ls2) = a*b + dot ls1 ls2
p6tests = [[0,0,1] `dot` [0,1,0] == 0]


-- Problem 7, increasing
increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [a] = True
increasing (a:ls) = if (head ls) > a then (increasing ls) else False
p7tests = [increasing "ABCD", not $ increasing [100,99..1]]


-- Problem 8, decimate
decimate :: [a] -> [a]
decimate ls = helper ls 1
    where
        helper [] _ = [] 
        helper (a:ls) i = if (mod i 10) /= 0 then (a) : (helper ls (i+1)) else helper ls (i+1)
p8tests = [decimate [1..21] == [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,21]]




-- Problem 9, encipher
encipher :: Eq a => [a] -> [b] -> [a] -> [b]
encipher _ _ [] = []
encipher ls1 ls2 (a:ls3) = helper ls1 ls2 a : encipher ls1 ls2 ls3
    where
        helper (x:ls1) (y:ls2) a = if (x == a) then y else helper ls1 ls2 a
p9tests = [encipher ['A'..'Z'] ['a'..'z'] "THIS" == "this"]

-- Problem 10, prefixSum
prefixSum :: (Num a) => [a] -> [a]
prefixSum [] = []
prefixSum [a] = [a]
prefixSum (a:b:ls) = a: prefixSum ((a+b):ls)
p10tests = [prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55], prefixSum [2, 5] == [2, 7]]

-- Problem 11, select
select :: (t -> Bool) -> [t] -> [a] -> [a]
select _ _ [] = []
select _ [] _ = []
select pred (a:ls1) (b:ls2) = if pred a then b:select pred ls1 ls2 else select pred ls1 ls2
p11tests = [select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz", select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26] == [1,2,3,4,5,6,7]]

-- Problem 12, numbers
numbers :: [Int] -> Int
numbers [] = 0
numbers [a] = a
numbers (a:ls) = helper a ls
    where helper final [] = final
          helper num (a:ls) = helper (num * 10 + a) ls
p12tests = [ numbers [1..4] == 1234]

tests = [p1tests,p2tests,p3tests,p35tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests]
likelyCorrect = and $ map and tests