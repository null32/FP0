import Data.List

-- 1
-- Определите функцию, возвращающую последний элемент списка.
--

_last l = if null (tail l) then head l else _last (tail l)

-- 3
-- Определите функцию, которая разделит исходный список из целых чисел на
-- два списка: список положительных чисел и список отрицательных чисел.

_split l = (filter (\x -> x > 0) l, filter (\x -> x < 0) l)

-- 11
-- Определите функции, осуществляющие преобразования
-- между видами (a b c) и (((а) b) с).
--



-- 19
-- Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно
-- множество подмножеством другого.
-- Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

_subset small big =
    if null small
    then True
    else
        if elem (head small) big
        then _subset (tail small) (delete (head small) big)
        else False

_selfSubset small big = _subset small big && not (null small) && length small /= length big

-- 26
-- Реализовать алгоритм сортировки слиянием.

_mergeSort [] = []
_mergeSort [l] = [l]
_mergeSort l =
    _merge (_mergeSort firstHalf) (_mergeSort secondHalf) where
        firstHalf = take ((length l) `div` 2) l
        secondHalf = drop ((length l) `div` 2) l

_merge f [] = f
_merge [] s = s
_merge (f:ft) (s:st)
  | f < s     = f:(_merge ft (s:st))
  | otherwise = s:(_merge (f:ft) st)


-- 2.1
-- Для заданного N построить код Грея.

_grayCode n =
    if n == 1
    then [[0], [1]]
    else do
        let prev = _grayCode (n - 1)
        let first = map (\x -> [0] ++ x) prev
        let second = map (\x -> [1] ++ x) (reverse prev)
        first ++ second

-- 3.1
-- Реализовать генератор деревьев, чтобы выдаваемые им деревья имели
-- количество вершин, точно соответствующее числу, указанному в его
-- первом аргументе.




_test _in _out = do
    putStr "  -> "
    print _in
    putStr "  <- "
    print _out

main = do
    putStrLn "Test #1"
    let someList = [1,2,3,4,5]
    _test someList (_last someList)

    putStrLn "Test #3"
    let someList = [-3,-2,-1,0,1,2,3]
    _test someList (_split someList)

    putStrLn "Test #19.1"
    let someSmall = [1,2,3]
    let someBig = [5,3,4,1,2,6]
    let otherBig = [2,3,4]
    _test [someSmall, someBig] (_subset someSmall someBig)
    _test [someSmall, otherBig] (_subset someSmall otherBig)

    putStrLn "Test #19.2"
    let someSmall = [1,2,3]
    let someBig = [1,2,3]
    let otherBig = [1,2,3,4]
    _test [someSmall, someBig] (_selfSubset someSmall someBig)
    _test [someSmall, otherBig] (_selfSubset someSmall otherBig)

    putStrLn "Test #26"
    let someList = [5,1,4,3,2]
    _test someList (_mergeSort someList)

    putStrLn "Test #2.1"
    let n = 3
    _test n (_grayCode n)
