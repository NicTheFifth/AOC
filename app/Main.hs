{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Maybe
import Data.List
import Data.Bifunctor
import Data.Bool
import Text.Show.Functions
import GHC.Float
import Data.Bits
import Text.Regex.TDFA
import qualified Data.Ord

days :: [(String, String -> String)]
days = [("day0", day0),
        ("day1", day1),
        ("altday1", altday1),
        ("day2", day2),
        ("altday2", altday2),
        ("day3", day3),
        ("altday3", altday3),
        ("day4", day4),
        ("altday4", altday4),
        ("day5", day5),
        ("altday5", altday5),
        ("day6", day6),
        ("altday6", altday6),
        ("day7", day7),
        ("altday7", altday7),
        ("day8", day8),
        ("day9", day9),
        ("day11", day11),
        ("altday11", altday11),
        ("day13", day13),
        ("altday13", altday13),
        ("day14", day14),
        ("day17", day17),
        ("day19", day19)]

main :: IO ()
main = do
  putStrLn "Select which day you want to solve (format day#, no cap or altday#)."
  day <- getLine
  file <- readFile ("days/"++ parseDay day ++ ".txt")
  putStr $ findFunction day file

parseDay :: String -> String
parseDay input | "alt" `isPrefixOf` input = drop 3 input
               | otherwise = input

findFunction :: String -> String -> String
findFunction day = fromJust $ lookup day days

day0 :: String -> String
day0 = id

--util

toInt :: String -> Int
toInt x = read x :: Int

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn cond = second (drop 1) . break (cond==)

splitOnEach :: Eq a => a -> [a] -> [[a]]
splitOnEach cond input | fstBreak == input = [fstBreak]
                       | otherwise = fstBreak : splitOnEach cond sndBreak
  where
    (fstBreak,sndBreak) =  splitOn cond input

get :: Int -> Int -> [[a]] -> a
get a b c = c !! a !! b

repeatF :: Int -> (a -> a) -> a -> a
repeatF 0 _ = id
repeatF a f = repeatF (a-1) f . f

--Day 1
day1parser :: String -> [[Int]]
day1parser = map (sort . map read) . transpose . map words . lines

day1 :: String -> String
day1 = show . solve . day1parser
  where
    solve :: [[Int]] -> Int
    solve [[], []] = 0
    solve [a:b, c:d] = abs (a-c) + solve [b,d]
    solve _ = error "we ded"

altday1 :: String -> String
altday1 = show . solve . clean . day1parser
  where
    clean :: [[Int]] -> [[Int]]
    clean [a,b] = [nub a, b]
    clean _ = error "we ded"
    solve :: [[Int]] -> Int
    solve [[],_] = 0
    solve [a:b, c] = length (elemIndices a c) * a + solve [b,c]
    solve _ = error "we ded"

--Day 2
day2parser :: String -> [[Int]]
day2parser = map (map read . words) . lines

data Cond = Unset | Unsafe | Set Dir Current | Current Current deriving Show
type Dir = Int -> Int -> Bool
type Safe = Bool
type Current = Int

day2 :: String -> String
day2 = show . sum . map (getCount . foldr getNext Unset) . day2parser
  where
    getCount :: Cond -> Int
    getCount Unsafe = 0
    getCount _ = 1
    getNext :: Int -> Cond -> Cond
    getNext _ Unsafe = Unsafe
    getNext b Unset = Current b
    getNext c (Current b) = bool (bool Unsafe (Set (>) c) (valid (>) b c)) (Set (<) c) (valid (<) b c)
    getNext c (Set f b) = bool  Unsafe (Set f c) (valid f b c)
    valid f b c = b `f` c && abs (b-c) < 4

type AltCond = [Skipable Cond]
data Skipable a = Skipped a | Unskipped a | UnSafe deriving Eq

altday2 :: String -> String
altday2 = show . sum .  map (getCount . foldr getNext [Unskipped Unset]) . day2parser
  where
    getCount :: AltCond -> Int
    getCount conds = bool 1 0  (all isUnsafe conds)
    isUnsafe UnSafe = True
    isUnsafe _ = False
    getNext :: Int -> AltCond -> AltCond
    getNext _ [] = []
    getNext a conds = foldr (step a) [] conds

    step :: Int -> Skipable Cond -> AltCond -> AltCond
    step a (Unskipped Unset) list = Skipped Unset : Unskipped (Current a) : list
    step a (Unskipped (Current b)) list = Skipped (Current b) : merge (bool (bool Unsafe (Set (>) a) (valid (>) a b)) (Set (<) a) (valid (<) a b)) Unskipped  : list
    step a (Unskipped (Set f b)) list = Skipped (Set f b) : merge (bool  Unsafe (Set f a) (valid f a b)) Unskipped  : list
    step a (Skipped Unset) list = Skipped (Current a) : list
    step a (Skipped (Current b)) list = merge (bool (bool Unsafe (Set (>) a) (valid (>) a b)) (Set (<) a) (valid (<) a b)) Skipped  : list
    step a (Skipped (Set f b)) list = merge (bool  Unsafe (Set f a) (valid f a b)) Skipped : list
    step a UnSafe list = list
    step a _ list = list

    merge Unsafe _ = UnSafe
    merge cond a = a cond

    valid f b c = b `f` c && abs (b-c) < 4

--Day3

day3 :: String -> String
day3 = show . sum . parse . concat . lines
  where
    parse :: String -> [Int]
    parse [] = []
    parse ('m':'u':'l':'(':x:',':a:')':rest) = (read [x] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:',':a:b:')':rest) = (read [x] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:',':a:b:c:')':rest) = (read [x] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:')':rest) = (read [x,y] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:b:')':rest) = (read [x,y] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:b:c:')':rest) = (read [x,y] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:')':rest) = (read [x,y,z] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:b:')':rest) = (read [x,y,z] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:b:c:')':rest) = (read [x,y,z] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse(_:rest) = parse rest

altday3 :: String -> String
altday3 = show . sum . parse . concat . lines
  where
    parseDont [] = []
    parseDont ('d':'o':'(':')':rest) = parse rest
    parseDont (_:rest) = parseDont rest
    parse :: String -> [Int]
    parse [] = []
    parse ('d':'o':'n':'\'':'t':'(':')':rest) = parseDont rest
    parse ('m':'u':'l':'(':x:',':a:')':rest) = (read [x] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:',':a:b:')':rest) = (read [x] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:',':a:b:c:')':rest) = (read [x] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:')':rest) = (read [x,y] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:b:')':rest) = (read [x,y] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:',':a:b:c:')':rest) = (read [x,y] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:')':rest) = (read [x,y,z] :: Int) * (read [a] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:b:')':rest) = (read [x,y,z] :: Int) * (read [a,b] :: Int) : parse rest
    parse ('m':'u':'l':'(':x:y:z:',':a:b:c:')':rest) = (read [x,y,z] :: Int) * (read [a,b,c] :: Int) : parse rest
    parse(_:rest) = parse rest

--Day 4

day4 :: String -> String
day4 = show . sum . map (sum . map count) . genAll . lines
  where
    genAll xs = [diag xs [], xs, transpose xs, diag (map reverse xs) []]
    diag' = id
    count [] = 0
    count ('S':'A':'M':'X':xs) = 1 + count ('X':xs)
    count ('X':'M':'A':'S':xs) = 1 + count ('S':xs)
    count (_:xs) = count xs

diag :: [[a]] -> [[a]] -> [[a]]
diag [] xs = xs
diag (x:xs) ys = z: diag xs zs
  where
    (z:zs) = combine x ys
    combine [] a = a
    combine (a:as) [] = [a] : combine as []
    combine (a:as) (b:bs) = (a:b) : combine as bs


altday4 :: String -> String
altday4 input = show xmas
  where
    square = lines input
    (x,y) = (length square, length (transpose square))
    as = [(i,indices) | i <- [1..x-2], let indices = filter (\z -> z < y-1 && 0<z) $ elemIndices 'A' (square !! i)]
    check :: String -> Bool
    check "MS" = True
    check "SM" = True
    check _ = False

    xmas = length [1 | (i, indices) <- as,
                        i' <- indices,
                        check [square !! (i+1) !! (i'+1), square !! (i-1) !! (i'-1)],
                        check [square !! (i-1) !! (i'+1), square !! (i+1) !! (i'-1)]
                      ]

-- Day 5

day5 :: String -> String
day5 input = show $ sum centerNumbers
  where
    (rules, tests) =  parseTests $ parseRules $ splitOn "" (lines input)
    parseRules :: ([String], c) -> ([(String,String)], c)
    parseRules = first (map (splitOn '|'))
    parseTests :: (c, [String]) -> (c, [[String]])
    parseTests = second (map (splitOnEach ','))
    validTests = filter (valid . reverse) tests
    valid :: [String] -> Bool
    valid [] = True
    valid (a:xs) = found a xs && valid xs
    found :: String -> [String] -> Bool
    found a xs | isNothing (lookup a rules) = True
               | otherwise = all ((`notElem` xs) . snd) (filter (\x -> a == fst x) rules)
    centerNumbers = map (\x -> toInt (x !! (length x `div` 2))) validTests

altday5 :: String -> String
altday5 input = show $ sum (getCentres validTests) + sum (getCentres validifiedTests)
  where
    (rules, tests) =  parseTests $ parseRules $ splitOn "" (lines input)
    parseRules :: ([String], c) -> ([(String,String)], c)
    parseRules = first (map (splitOn '|'))
    parseTests :: (c, [String]) -> (c, [[String]])
    parseTests = second (map (splitOnEach ','))
    (validTests,invalidTests) = partition (valid . reverse) tests
    valid :: [String] -> Bool
    valid [] = True
    valid (a:xs) = found a xs && valid xs
    found :: String -> [String] -> Bool
    found a xs | isNothing (lookup a rules) = True
               | otherwise = all ((`notElem` xs) . snd) (filter (\x -> a == fst x) rules)
    getCentres = map (\x -> toInt (x !! (length x `div` 2)))
    validifiedTests = map (reverse . validify) (reverse invalidTests)
    validify [] = []
    validify (a:as) | isNothing (lookup a rules) = a : validify as
                    | otherwise = a : validify as --To fix

--Day 6

data Direction = DUp | DDown | DLeft | DRight deriving (Show,Eq)

instance Enum Direction where
  toEnum int = get $ abs $ int `mod` 4
    where
      get 0 = DUp
      get 1 = DRight
      get 2 = DDown
      get 3 = DLeft
      get _ = error "no enum exists"
  fromEnum DUp = 0
  fromEnum DRight = 1
  fromEnum DDown = 2
  fromEnum DLeft = 3

              --(Y, X)
type Position = (Int, Int)

data Board = Board {
  direction :: Direction,
  position :: Position,
  board :: [String]
}

day6 :: String -> String
day6 input = show $ step initBoard
  where
    initBoard = Board DUp initPos (lines input)
    initPos = (initX, fromJust $ elemIndex '^' (lines input !! initX))
    initX = fromJust $ findIndex ('^' `elem`) (lines input)

step :: Board -> Int
step bboard@Board {direction, position, board} | end = sum (map (length . elemIndices 'X') (replace 'X' position))
                                               | otherwise = step nextBoard
  where
    end = offBoard direction || cycle direction
    (ys,xs) = position
    y = length board -1
    x = length (head board) -1
    offBoard DDown = y == ys
    offBoard DUp = ys == 0
    offBoard DLeft = xs == 0
    offBoard DRight = xs == x
    cycle DDown | ys == y-1 = False
                | get (ys+1) xs board == 'X' && get (ys+2) xs board == '#' = True
                | otherwise = False
    cycle DUp | ys == 1 = False
              | get (ys-1) xs board == 'X' && get (ys-2) xs board == '#' = True
              | otherwise = False
    cycle DLeft | xs == 1 = False
                | get ys (xs-1) board == 'X' && get ys (xs-2) board == '#' = True
                | otherwise = False
    cycle DRight | xs == x-1 = False
                 | get ys (xs+1) board == 'X' && get ys (xs+2) board == '#' = True
                 | otherwise = False
    nextBoard | blocked direction = Board (succ direction) position board
              | otherwise = Board direction (newPosition direction) newBoard

    blocked DDown = get (ys +1) xs board == '#'
    blocked DUp = get (ys -1) xs board == '#'
    blocked DRight = get ys (xs+1) board == '#'
    blocked DLeft = get ys (xs-1) board == '#'

    newPosition DDown = (ys+1, xs)
    newPosition DUp = (ys-1, xs)
    newPosition DRight = (ys, xs+1)
    newPosition DLeft = (ys, xs-1)

    replace char (ycoord, xcoord) = take ycoord board ++ (take xcoord (board !! ycoord) ++ char : drop (xcoord+1) (board !! ycoord)) : drop (ycoord+1) board
    newBoard = replace 'X' position

altday6 :: String -> String
altday6 input = show $ stepalt initBoard 0
  where
    initBoard = Board DUp initPos (lines input)
    initPos = (initX, fromJust $ elemIndex '^' (lines input !! initX))
    initX = fromJust $ findIndex ('^' `elem`) (lines input)

stepalt :: Board -> Int -> Int
stepalt bboard@Board {direction, position, board} count | end = count
                                                        | otherwise = stepalt nextBoard nextCount
  where
    end = offBoard direction || cycle direction
    (ys,xs) = position
    y = length board -1
    x = length (head board) -1
    offBoard DDown = y == ys
    offBoard DUp = ys == 0
    offBoard DLeft = xs == 0
    offBoard DRight = xs == x
    cycle DDown | ys == y-1 = False
                | get (ys+1) xs board == 'X' && get (ys+2) xs board == '#' = True
                | otherwise = False
    cycle DUp | ys == 1 = False
              | get (ys-1) xs board == 'X' && get (ys-2) xs board == '#' = True
              | otherwise = False
    cycle DLeft | xs == 1 = False
                | get ys (xs-1) board == 'X' && get ys (xs-2) board == '#' = True
                | otherwise = False
    cycle DRight | xs == x-1 = False
                 | get ys (xs+1) board == 'X' && get ys (xs+2) board == '#' = True
                 | otherwise = False
    nextBoard | blocked direction = Board (succ direction) (newPosition (succ direction)) newBoard
              | otherwise = Board direction (newPosition direction) newBoard

    blocked DDown = get (ys +1) xs board == '#'
    blocked DUp = get (ys -1) xs board == '#'
    blocked DRight = get ys (xs+1) board == '#'
    blocked DLeft = get ys (xs-1) board == '#'

    newPosition DDown = (ys+1, xs)
    newPosition DUp = (ys-1, xs)
    newPosition DRight = (ys, xs+1)
    newPosition DLeft = (ys, xs-1)

    replace char (ycoord, xcoord) boarda = take ycoord boarda ++ (take xcoord (boarda !! ycoord) ++ char : drop (xcoord+1) (boarda !! ycoord)) : drop (ycoord+1) boarda
    newBoard = replace 'X' position board
    nextCount | get ys xs board == 'X' = count + 1
             | otherwise = count


--Day 7

parseDay7 :: String -> [Day7]
parseDay7 = map (bimap toInt (map toInt .  words) . splitOn ':') . lines

type Day7 = (Goal, [Int])
type Goal = Int

day7 :: String -> String
day7 = show . sum . map solve . parseDay7
  where
    solve :: Day7 -> Int
    solve (goal, list) | elem goal $ foldr tree [] (reverse list) = goal
                       | otherwise = 0
    tree a [] = [a]
    tree a [b] = [a + b, a * b]
    tree a (b:bs) = a+b : a * b : tree a bs

altday7 :: String -> String
altday7 = show . sum . map solve . parseDay7
  where
    solve :: Day7 -> Int
    solve (goal, list) | elem goal $ foldr tree [] (reverse list) = goal
                       | otherwise = 0
    tree a [] = [a]
    tree a [b] = [a + b, a * b, toInt (show b ++ show a)]
    tree a (b:bs) = a+b : a * b : toInt (show b ++ show a) : tree a bs

--Day 8

data Day8 = Day8 {
  size::(Int, Int),
  groupedNodes::[[Position]]
} deriving Show

parseDay8 :: String -> Day8
parseDay8 input = Day8 size' groupedNodes'
  where
    field = lines input
    size' = (length field, length (head field))
    nodesrows = findIndices (not . all ('.'==)) field
    coords = concatMap (\x -> map (x,) $ findIndices ('.'/=) (field!!x)) nodesrows
    nodes' = map (\(x,y) -> (field !! x!!y, (x,y))) coords
    grouped = groupBy (\x y -> fst x == fst y) $ sortBy (\(a,_) (b,_) -> compare a b) nodes'
    groupedNodes' = map (map snd) grouped

day8 :: String -> String
day8 = show . solve . parseDay8
  where
    solve :: Day8 -> Int
    solve day8data =  length $ nub $ concatMap (filter (inBounds (size day8data)) . compute) (groupedNodes day8data)
    inBounds :: Position -> Position -> Bool
    inBounds (x,y) (x',y') = 0<=x' && 0<=y' && x' < x && y' < y
    compute :: [Position] -> [Position]
    compute [a] = []
    compute (a:as) = concatMap (getUpDown a) as ++ compute as
    getUpDown :: Position -> Position -> [Position]
    getUpDown (x,y) (x',y') = [(x+x-x', y+y-y'), (x'-(x-x'), y'-(y-y'))]

--Day 9
-- (amount, [(value, amounts)])
parseDay9 :: String -> (Int, [(Int, Int)])
parseDay9 = comb 0 True . concatMap (map (\x -> toInt [x])) . lines

comb :: Int -> Bool -> [Int] -> (Int, [(Int, Int)])
comb _ _ [] = (0,[])
comb n True (0:bs) = comb n False bs
comb n False (0:bs) = comb n True bs
comb n True (a:bs) = bimap (a+) ((n, a) :) (comb (n+1) False bs)
comb n False (a:bs) = second ((-1, a) :) (comb n True bs)

day9 :: String -> String
day9 = show . addDay9 0 . solve . parseDay9
  where
    solve a = uncurry solveDay9 a (reverse (snd a))

addDay9 :: Int -> [(Int, Int)] -> Int
addDay9 _ [] = 0
addDay9 n ((num, amount) : as) = sum (zipWith (*) [n..(n+amount-1)] (replicate amount num)) + addDay9 (n+amount) as

solveDay9 :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
solveDay9 0 _ _ = []
solveDay9 n as ((-1,_):vs) = solveDay9 n as vs
solveDay9 n ((-1,aamount):as) ((v,vamount):vs) | vamount >= n = [(v,vamount)]
                                               | aamount == vamount = (v,vamount) : solveDay9 (n-vamount) as vs
                                               | aamount > vamount = (v,vamount) : solveDay9 (n-vamount) ((-1,aamount-vamount):as) vs
                                               | otherwise = (v,aamount) : solveDay9 (n-aamount) as ((v,vamount-aamount) : vs)
solveDay9 n ((a,aamount):as) vs = (a,aamount) : solveDay9 (n-aamount) as vs
solveDay9 _ _ _ = error "fucky"

--Day 11

parseDay11 :: String -> [Int]
parseDay11 = concatMap (map toInt . words) . lines

day11 :: String -> String
day11 = show . length . repeatF 25 blink . parseDay11

blink :: [Int] -> [Int]
blink [] = []
blink (0:xs) = 1: blink xs
blink (x:xs) | odd (floor index) = fs : sn : blink xs
             | otherwise = (x * 2024) : blink xs
  where
    index = logBase 10 (int2Float x)
    getTen = float2Int (10 ** int2Float (ceiling $ index / 2))
    (fs, sn) = divMod x getTen

altday11 :: String -> String
altday11 = show . length . repeatF 75 blink . parseDay11

--Day 13

data Day13 = Day13 {
  a :: Button,
  b :: Button,
  goal :: Position
} deriving Show

type Button = (Int,Int)

parseDay13 :: String -> [Day13]
parseDay13 = map parse . splitOnEach [] . lines
  where
    parse [a,b,goal] = Day13 (parseButton $ words a) (parseButton $ words b) (parseGoal $ words goal)
    parseButton a = (parseX (a!!2), parseY (a!!3))
    parseX a = toInt $ drop 2 $ take (length a -1) a
    parseY a = toInt $ drop 2 a
    parseGoal g = (parseX (g!!1), parseY (g!!2))

day13 :: String -> String
day13 = show . calc . parseDay13
  where
    calc = sum . map (getMin . valid)
    valid :: Day13 -> [Int]
    valid (Day13 a b goal) = [bs + 3*as | as <- [0..100], bs <- [0..100], fst a*as + fst b*bs == fst goal, snd a*as + snd b * bs == snd goal]
    getMin [] = 0
    getMin a = minimum a

altday13 :: String -> String
altday13 = show . calc . parseDay13
  where
    calc = sum . map valid
    valid' day as bs | isEq day as bs = as*3 + bs
                     | bs == 0 = 0
                     | isGreater day as bs = valid' day as (bs-1)
                     | otherwise = valid' day (as+1) bs
    valid d@(Day13 a b goal) = valid' d 0 (getLeast b goal)
    getLeast :: (Int,Int) -> (Int,Int) -> Int
    getLeast (a,b) (c,d) = min ((10000000000000 + c)`div` a) ((10000000000000+d) `div` b)
    isEq (Day13 a b goal) as bs = fst a*as + fst b*bs == fst goal && snd a*as + snd b * bs == snd goal
    isGreater (Day13 a b goal) as bs = fst a*as + fst b*bs >= fst goal && snd a*as + snd b * bs >= snd goal

--Day 14

data Bot = Bot {
  pos:: Position,
  move :: (Int, Int)
}

parseDay14 :: String -> [Bot]
parseDay14 =  map (botify . splitOn ' ') . lines
  where
    botify :: (String,String) -> Bot
    botify (pos,dir) = Bot (getIntInt pos) (getIntInt dir)
    getIntInt :: String -> (Int, Int)
    getIntInt = bimap toInt toInt . splitOn ',' . drop 2

day14 :: String -> String
day14 = show . calc . map (day14step 100) . parseDay14
  where
    isQuad f1 f2 (x,y) = x `f1` 50 && y `f2` 51
    calc positions = product [length (filter (isQuad a b) positions) | a <- [(<),(>)], b<- [(<),(>)]]

day14step :: Int -> Bot -> Position
day14step n (Bot (x,y) (dx,dy)) = ((x+n*dx) `mod` 101, (y+n*dy) `mod` 103)

--Day 17

parseDay17 :: [String] -> Day17
parseDay17 input = Day17 0 reg op []
  where
    (unparsedReg, unparsedOp) = splitOn [] input
    [[a],[b],[c]] = map (map toInt . drop 2 . words) unparsedReg
    reg = (a,b,c)
    [[op]] = map (map (map toInt . splitOnEach ',') . drop 1 . words) unparsedOp

day17 :: String -> String
day17 = intercalate "," . map show . operate . parseDay17 . lines

fstTrip :: (a, b, c) -> a
fstTrip (a,_,_) = a

sndTrip :: (a, b, c) -> b
sndTrip (_,b,_) = b

trdTrip :: (a, b, c) -> c
trdTrip (_,_,c) = c

type Reg = (Int, Int, Int)

combo :: Int -> Reg -> Int
combo 4 (a,_,_) = a
combo 5 (_,b,_) = b
combo 6 (_,_,c) = c
combo 7 _ = error "fucky"
combo num _ = num

data Day17 = Day17 {
  opPos :: Int,
  reg :: Reg,
  inst :: [Int],
  output :: [Int]
}

operate :: Day17 -> [Int]
operate day17@(Day17 opPos reg inst output) | length inst <= opPos = reverse output
                                            | otherwise = operate $ doOp (inst !! opPos) (inst !! (opPos+1)) day17

doOp :: Int -> Int -> Day17 -> Day17
doOp 0 operand day17 = day17{opPos = opPos day17 + 2, reg = (adv (reg day17) operand, sndTrip (reg day17), trdTrip (reg day17))}
doOp 1 operand day17 = day17{opPos = opPos day17 + 2, reg = (fstTrip (reg day17), xor (sndTrip (reg day17)) operand, trdTrip (reg day17))}
doOp 2 operand day17 = day17{opPos = opPos day17 + 2, reg =(fstTrip (reg day17), combo operand (reg day17) `mod` 8, trdTrip (reg day17))}
doOp 3 operand day17 = day17{opPos = bool operand (opPos day17 + 2) (fstTrip (reg day17) == 0)}
doOp 4 operand day17 = day17{opPos = opPos day17 + 2,reg = (fstTrip (reg day17), xor (sndTrip (reg day17)) (trdTrip (reg day17)), trdTrip (reg day17))}
doOp 5 operand day17 = day17{opPos = opPos day17 + 2, output = (combo operand (reg day17) `mod` 8) : output day17}
doOp 6 operand day17 = day17{opPos = opPos day17 + 2, reg = (fstTrip (reg day17), adv (reg day17) operand, trdTrip (reg day17))}
doOp 7 operand day17 = day17{opPos = opPos day17 + 2, reg = (fstTrip (reg day17), sndTrip (reg day17), adv (reg day17) operand)}
doOp _ _ _ = error "Fuckywucky"

adv :: Reg -> Int -> Int
adv reg op = fstTrip reg `div` (2^combo op reg)

--Day 19

data Day19 = Day19 {
  regex::String,
  inputs::[String]
} deriving Show

parseDay19 :: ([String], [String]) -> Day19
parseDay19 ([unparsedRegEx], parsedInput) = Day19 regEx parsedInput
  where
    regEx = '^':'(':'(' : intercalate ")|(" (sortOn (Data.Ord.Down . length) (map (\x -> take (length x - 1) x) $ words unparsedRegEx)) ++ "))+$"

day19 :: String -> String
day19 = show . length . valid . parseDay19 . splitOn [] . lines
    where
      valid (Day19 regex inputs) = filter (\x -> ((x =~ regex) :: Bool)) inputs