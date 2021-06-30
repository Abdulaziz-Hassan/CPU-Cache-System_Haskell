data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)

data Tag = T Int deriving (Show, Eq)

data Data a = D a deriving (Show, Eq)

data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
---General

convertBinToDec :: Integral a => a -> a

convertBinToDec x = converter 0 x

converter _ 0 = 0
converter counter x = if mod  x 10 == 0 then converter (counter+1) (div x 10) else 2^counter + converter (counter+1) (div x 10) 

replaceIthItem :: t -> [t] -> Int -> [t]

replaceIthItem _ [] _ = error "Empty Tree"
replaceIthItem character (x:xs) index =   if index /= 0 then x:replaceIthItem character (xs) (index-1) else character:xs

splitEvery :: Int -> [a] -> [[a]]

splitEvery num (x:xs) = splitEveryHelper num (x:xs) 1 [] []

splitEveryHelper num [] index acc bigacc =  if (length acc) /=0 then (bigacc ++ [acc]) else bigacc

splitEveryHelper num (x:xs) index acc bigacc = if num == index then splitEveryHelper num (xs) 1 [] (bigacc ++ [(acc ++ [x])]) 
    else splitEveryHelper num (xs) (index+1)  (acc ++ [x]) bigacc

--logBase2 :: Floating a => a -> a --TODO

logBase2 a = logBase2Helper a 0

logBase2Helper 1 num = num

logBase2Helper a num = logBase2Helper (div a 2) (num+1)

fillZeros :: [Char] -> Int -> [Char]

fillZeros (x:xs) num = if(num == 0) then (x:xs) else fillZeros ('0':x:xs) (num-1)

--convertAddress:: (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)

convertAddress binAddress bitsNum map = if(map == "directMap") then( ( div ( fromInteger binAddress)  (10^(fromInteger bitsNum)),  mod (fromInteger binAddress)  (10^( fromInteger bitsNum)) )) 
else if(map == "fullyAssoc") then(( fromIntegral binAddress),0)
else ((div ( fromInteger binAddress)  (10^( fromInteger bitsNum))),(mod ( fromInteger binAddress) (10^( fromInteger bitsNum))))											

--getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a

--fromIntegral :: (Num b, Integral a) => a -> b
getNumBits  bitsNum map cache  =  if(map == "directMap") then logBase2 (length cache) 
	else if (map =="setAssoc" ) then logBase2  ( fromInteger bitsNum) else  0

---getDataFromCache

getDataFromCache:: (Integral b, Eq a) =>[Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache stringAddress cache "directMap" bitsNum
    |tag == binTag && validBit == True = Out (retrievedData,0)
    |otherwise = NoOutput
    where
        (binTag,binIndex) = convertAddress (read stringAddress :: Integer) (fromIntegral bitsNum) "directMap"
        index = convertBinToDec binIndex
        (It (T tag) (D retrievedData) validBit order) = cache!!index

getDataFromCache stringAddress [] "fullyAssoc" _ = NoOutput

getDataFromCache stringAddress cache "fullyAssoc" bitsNum = getDataFromCacheHelper (read stringAddress :: Int) cache "fullyAssoc" bitsNum 0


getDataFromCache stringAddress cache "setAssoc" bitsNum = getDataFromCacheS (extractTag stringAddress bitsNum) (splitEvery (div (length cache) 2^bitsNum) cache) (convertBinToDec (read (extractLastN (fromIntegral bitsNum) stringAddress) :: Int))

getDataFromCacheS tag newCache index = getDataFromCacheSH (read tag :: Int) (newCache!!index) 0

getDataFromCacheSH tag ((It (T t) (D d) validBit order):xs) counter 
    |tag == t && validBit == True = Out (d,counter)
    |otherwise = getDataFromCacheSH tag xs (counter + 1)

getDataFromCacheSH tag [] _ = NoOutput
        

getDataFromCacheHelper address ((It (T tag) (D curData) validBit order):xs) "fullyAssoc" bitsNum counter 
    |address == (convertBinToDec tag) && validBit == True = Out (curData,counter)
    |otherwise = getDataFromCacheHelper address xs "fullyAssoc" bitsNum (counter + 1)

getDataFromCacheHelper address [] "fullyAssoc" bitsNum counter = NoOutput

---replaceInCache					
										
replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])	
replaceInCache tag idx memory oldCache "directMap" bitsNum = (getIthItem memory (createAddress tag idx (fromIntegral bitsNum)),replaceIthItem (createNewItem tag (getIthItem memory (createAddress tag idx (fromIntegral bitsNum)))) oldCache (convertBinToDec idx))
 

replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = 
	(
		getIthItem memory (convertBinToDec tag),
		replaceIthItem 
			(createNewItem tag (getIthItem memory (convertBinToDec tag)))
			(incrementOrder oldCache) 
			(getPriorityByIndex oldCache)
	)

  

replaceInCache tag idx memory oldCache "setAssoc" bitsNum = 
	(
		getIthItem memory (createAddress tag idx (fromIntegral bitsNum)),
		concat (helper1 tag idx memory oldCache (fromIntegral bitsNum))
	)

---helper Functions        

extractTag stringAddress bitsNum 
    |bitsNum > 0 = extractTag (init stringAddress) (bitsNum - 1)
    |otherwise = stringAddress

loopOverList :: [a] -> [a] -> [a]
loopOverList []     []     = []
loopOverList xs     []     = xs
loopOverList []     ys     = ys
loopOverList (x:xs) (y:ys) = loopOverList xs ys

extractLastN :: Int -> [a] -> [a]
extractLastN n xs = loopOverList (drop n xs) xs


helper1 tag idx memory oldCache bitsNum = 
	replaceIthItem 
		(helper2 tag idx memory oldCache bitsNum)
		(splitEvery (2^bitsNum) oldCache)
		(convertBinToDec idx)

helper2 tag idx memory oldCache bitsNum = 
	replaceInSubCache 
	(
		createNewItem 
			tag 
			(getIthItem memory (createAddress tag idx bitsNum))
	) 
	(helper3 tag idx oldCache bitsNum) 

helper3 tag idx oldCache bitsNum = 
	getIthItem 
		(splitEvery (2^bitsNum) oldCache) 
		(convertBinToDec idx)

getPriorityByIndex (x:xs) = getPriorityByIndexHelper xs x 0 0 

getPriorityByIndexHelper [] _ _ indexSoFar = indexSoFar

getPriorityByIndexHelper ((It _ _ _ _):_) (It _ _ False _) counter _ = counter

getPriorityByIndexHelper ((It _ _ False _):_) (It _ _ True _) counter _ = counter+1

getPriorityByIndexHelper ((It tag1 dta1 True order1):xs) (It tag2 dta2 True order2) counter indexSoFar = 
		if order1<=order2 then 
			getPriorityByIndexHelper xs (It tag2 dta2 True order2) (counter+1) indexSoFar
		else 
			getPriorityByIndexHelper xs (It tag1 dta1 True order1) (counter+1) (counter+1) 


incrementOrder [] = []

incrementOrder ((It tag dta bool order):xs) = 
	if bool==True then 
		[(It tag dta bool (order+1))] ++ incrementOrder xs 
	else 
		[(It tag dta bool order)] ++ incrementOrder xs

replaceInSubCache newItem subCache = replaceIthItem newItem (incrementOrder subCache) (getPriorityByIndex subCache)


getIthItem list index = list!!index
										
createAddress tag idx bitsNum = convertBinToDec (read (show tag ++ getIdx idx bitsNum) :: Int)

getIdx idx bitsNum = fillZeros (show idx) (bitsNum - length (show idx))

createNewItem tag memData = It (T tag) (D memData) True 0



---Implemented Functions

getData stringAddress cache memory cacheType bitsNum
    | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
    | otherwise = (getX x, cache)
    where
        x = getDataFromCache stringAddress cache cacheType bitsNum
        address = read stringAddress:: Int
        (tag, index) = convertAddress (fromIntegral address) (fromIntegral bitsNum) cacheType
getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
    where
    bitsNum = round(logBase2 numOfSets)
    (d, updatedCache) = getData addr cache memory cacheType bitsNum
    (prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets
