{-
Name: Timilsina, Sandesh
Net ID: stimilsina@unm.edu
-}
import Data.List
import Data.Function

-- Problem 1, bits2num
bits2num :: Num a => [Char] -> a
bits2num [] = 0
bits2num (x:ls) =  (helper x) * (2 ^ (length ls)) + (bits2num ls)
    where helper '0' = 0
          helper '1' = 1
          helper '2' = 2
          helper '3' = 3
          helper '4' = 4
          helper '5' = 5
          helper '6' = 6
          helper '7' = 7
          helper '8' = 8
          helper '9' = 9

p1tests = [bits2num "1011000" == 88]


-- Problem 2, num2bits
num2bits :: Integral a => a -> [Char]
num2bits x = "0" ++ helper x
    where helper 0 = "0"
          helper 1 = "1"
          helper num = helper (num `div` 2) ++ helper (num `mod` 2)
p2tests = [num2bits 87783 == "010101011011100111"]


-- Problem 3, variance3
variance :: (Num a, Fractional a) => [a] -> a
variance ls = (sum (helper1 ls mean)) / (fromIntegral (length ls))
    where helper1 list m = map (^ 2) (map (uncurry (-)) (zip list m))
          mean = map (\x-> (sum ls) / (fromIntegral (length ls))) ls



p3tests = [variance [1..10] == 8.25]


-- Problem 4, difference
difference :: Eq a => [a] -> [a] -> [a]
difference ls1 [] = ls1
difference ls1 (a:ls2) = difference (helper (nub' ls1) a) ls2
    where helper list letter = [x | x <-list, x /= letter]
          nub' [] = []
          nub' (a:ls) = let helper' x [] = []
                            helper' x (a:ls) = if (a == x) then helper' x ls else a : helper' x ls
                          in if (elem a ls) then a : (nub' (helper' a ls)) else a : (nub' ls)


p4tests = [difference "ABCD" "AD" == "BC",difference "ABCDCBA" "AD" == "BC"]


-- Problem 5, splits


splits ::  Ord a => [a] -> [([a], [a])]
splits ls = zip (map (\x->difference ls x) (helpNub (init (init (helper (length ls) ls))))) (helpNub (init (init (helper (length ls) ls))))
    where helper _ [] = []
          helper len list = (combinations (len-1) list) ++ if(len-1) /= 0 then (helper (len - 1) list) else [[]]
          combinations 0 list = [[]]
          combinations 1 list = [[x] | x <- list]
          combinations n list =
              let comb element = combinations (n-1) (tail (filter (>= element) list))
              in concat (map (\a -> [a:b | b <- comb a]) list)
          nub' []= []
          nub' (x:xs) = x : nub' (filter (/=x) xs)
          helpNub ls = map (\x -> nub' x) ls

--p5tests = [splits "abc" == [("c","ab"),("b","ac"),("bc","a"),("a","bc"),("ac","b"),("ab","c")]]
p5tests = [sort (splits "abc") == sort [("c","ab"),("b","ac"),("bc","a"),("a","bc"),("ac","b"),("ab","c")]]



-- Problem 6, argmin
argmin ::  (Ord a) => (t -> a) -> [t] -> t
argmin func (x:ls) = helper ls x
    where helper [] x = x 
          helper (x:list) current = if ((func x) < (func current)) then helper list x else helper list current

p6tests = [argmin length ["ABC","EF","GHIJ","K"] == "K"]


data Htree a = HLeaf Double a | HFork Double [a] (Htree a) (Htree a) deriving (Show, Eq)
-- Problem 7, bogus


merge u@(HLeaf x l) v@(HLeaf y r) = HFork (x+y) [l,r] u v
merge u@(HLeaf x l) v@(HFork y rs _ _) = HFork (x+y) (l:rs) u v
merge u@(HFork x ls _ _) v@(HLeaf y r) = HFork (x+y) (r:ls) u v
merge u@(HFork x ls _ _) v@(HFork y rs _ _) = HFork (x+y) (ls ++ rs) u v

instance (Ord a) => Ord (Htree a) where
    (HLeaf x _) < (HLeaf y _) = x < y
    (HLeaf x _) < (HFork y _ _ _) = x < y
    (HFork x _ _ _) < (HLeaf y _) = x < y
    (HFork x _ _ _) < (HFork y _ _ _) = x < y
    (HLeaf x _) <= (HLeaf y _) = x <= y
    (HLeaf x _) <= (HFork y _ _ _) = x <= y
    (HFork x _ _ _) <= (HLeaf y _) = x <= y
    (HFork x _ _ _) <= (HFork y _ _ _) = x <= y

-- encode character using Huffman coding tree
encode (HFork _ _ (HLeaf _ l) (HLeaf _ r)) c = if c == l then "0" else "1"
encode (HFork _ _ (HLeaf _ l) v@(HFork _ rs _ _)) c =
    if c == l then "0" else '1':(encode v c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HLeaf _ r)) c =
    if c == r then "1" else '0':(encode u c)
encode (HFork _ _ u@(HFork _ ls _ _) v@(HFork _ rs _ _)) c =
    if c `elem` ls then '0':(encode u c) else '1':(encode v c)

-- decode message using Huffman coding tree
decode t [] = []
decode t (x:xs) = loop t (x:xs)
    where loop (HLeaf _ l) xs = l:(decode t xs)
          loop (HFork _ _ u v) ('0':xs) = loop u xs
          loop (HFork _ _ u v) ('1':xs) = loop v xs


bogus :: Ord a => [(Double, a)] -> Htree a
bogus ls = helper (makeLeaf ls)
    where makeLeaf ls = map (\(a,b)->HLeaf a b) ls
          helpmin (left, right) = (abs ((sum (map (\(HLeaf num letter)->num) left)) - (sum (map (\(HLeaf num letter)->num) right))))
          helper [x] = x
          helper list = merge (helper subsetA) (helper subsetB)
            where (subsetA,subsetB) = argmin helpmin (splits list)
                

helpmin'' (left, right) = (abs ((sum (map (\(HLeaf num letter)->num) left)) - (sum (map (\(HLeaf num letter)->num) right))))


p7tests = let xs = [(0.30,'e'), (0.14,'h'), (0.1,'l'), (0.16,'o'), (0.05,'p'), (0.23,'t'), (0.02,'w')] in [(decode (bogus xs) $ concatMap (encode (bogus xs)) "hello") == "hello", concatMap (encode (bogus xs)) "hello" /= concatMap (encode (bogus xs)) "oellh"]



-- Problem 8, church
church :: Int -> (a -> a) -> a -> a
church 0 _ ls = ls
church n func ls = foldr (helper) ls [1..n]
    where helper a b = func b

p8tests = [church 4 tail "ABCDEFGH" == "EFGH", church 100 id 9001 == 9001]


data Btree a = BLeaf a | BFork (Btree a) (Btree a) deriving (Show, Eq)
-- Problem 9, trees




trees ls = concat (map helper subsets)
    where subsets = splits ls
          helper ([a], [b]) = [BFork (BLeaf a) (BLeaf b)]
          helper (ls1, [b]) = map (\x -> BFork x (BLeaf b)) (trees ls1)
          helper ([a], ls2) = map (\x -> BFork (BLeaf a) x) (trees ls2)
          helper (ls1, ls2) = concat (map (\b -> map (\a -> BFork a b) (trees ls1)) (trees ls2))


p9tests = [(trees "ABCDE") !! 114 == BFork (BLeaf 'E') (BFork (BFork (BLeaf 'A') (BFork (BLeaf 'C') (BLeaf 'B'))) (BLeaf 'D')), length (trees [0..4]) == 1680]


bases = "AGCT"
-- Problem 10, insertions
insertions :: String -> [String]
insertions ls = helper bases
    where helper [] = []
          helper str = (loop (take 1 str) 0 ((length ls) + 1) []) ++ helper (drop 1 str)
          loop letter index lastIndex acc= if index >= lastIndex then acc else [(take index ls) ++ letter ++ (drop index ls)] ++ loop letter (index +1) lastIndex acc



p10tests = [insertions "GC" == ["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"]]


-- Problem 11, deletions
deletions :: [a] -> [[a]]
deletions str = map (\(a,b)-> (take (b - 1) str) ++ (drop b str)) (zip str [1,2..])

p11tests = [deletions "AGCT" == ["GCT","ACT","AGT","AGC"]]


-- Problem 12, substitutions
substitutions :: String -> [String]
substitutions ls = helper bases
    where helper [] = []
          helper str = (loop (take 1 str) 0 (length ls) []) ++ helper (drop 1 str)
          loop letter index lastIndex acc= if index >= lastIndex then acc else [(take index ls) ++ letter ++ (drop (index+1) ls)] ++ loop letter (index +1) lastIndex acc 

p12tests = [substitutions "ACT" == ["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"]]


-- Problem 13, transpositions
transpositions :: [a] -> [[a]]
transpositions ls = helper 0 (length ls)
    where helper index len = if (index + 1) == len then [] else [(take index ls) ++ [ls !! (index+1)] ++ [ls !! index] ++ (drop (index +2) ls)] ++ helper (index + 1) len

p13tests = [transpositions "GATC" == ["AGTC","GTAC","GACT"]]


--p9tests sort is not working.
tests = [p1tests,p2tests,p3tests,p4tests,p5tests,p6tests,p7tests,p8tests,p9tests,p10tests,p11tests,p12tests,p13tests]
likelyCorrect = let results = [and t | t <- tests] in (and results, filter (not.snd) $ zip [1..] results)