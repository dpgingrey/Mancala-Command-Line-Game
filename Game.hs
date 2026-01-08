module Game where

import Data.Maybe (fromMaybe)
------------- Story One ---------------

type Index = Int
type Pit = (Index, Int) --(Index, Num of Marbles)
type Board = ([Pit], Pit, [Pit], Pit) --  P1 pits, P1 Store, P2 pits, P2 Store
data Player = P1 | P2 deriving (Eq,Show)
type Turn = Player
data Winner = Win Player | Tie deriving (Eq,Show)
type Move = Index
type Game = (Turn, Board) 

--Returns the index of the store for the player input.
store :: Player -> Index
store P1 = 7
store P2 = 0

--Returns the list of indexes of the pits associated with input player.
pits :: Player -> [Index]
pits P1 = [1..6]
pits P2 = [8..13]

--Returns the opposite of the input player.
opponent :: Player -> Player
opponent P1 = P2
opponent P2 = P1

--Takes a key, value, and an association list and changes the value to the given value at the key.
changeValue :: (Eq a, Num b) => a -> b -> [(a,b)] -> [(a,b)]  
changeValue targetKey newValue [] = error "Key not found in association list."
changeValue targetKey newValue ((key,value):lst) =  
    if key == targetKey
    then (key, newValue):lst
    else (key, value):(changeValue targetKey newValue lst)           

--Takes a key and returns the value at that location in the provided association list, but errors if it sees a nothing.
lookUpUnsafe :: (Eq a, Show  a, Num b) => a -> [(a,b)] -> b 
lookUpUnsafe key alist = case lookup key alist of
    Just value -> value
    Nothing    -> error $ (show key) ++ " is not a valid key." 

--Takes an index and a board and returns the amount of marbles at the location, errors if unfound.
boardLookUpUnsafe :: Index -> Board -> Int 
boardLookUpUnsafe index (p1Pits, (p1SIndex,p1SMarbles), p2Pits, (p2SIndex, p2SMarbles)) 
    | index == 0           = p2SMarbles
    | index == 7           = p1SMarbles
    | index `elem` pits P1 = lookUpUnsafe index p1Pits
    | otherwise            = lookUpUnsafe index p2Pits

---------------------------------------

------------- Story Two ----------------
-- Check who has won the game state, if anyone, with a function of type  Game -> Winner.

--Check if either player’s side is empty.
--If one side is empty:
--Add all marbles from the opposing side to that player’s store.
--Then compare the two stores to determine the winner, or a tie.
--If neither side is empty:
--The game should continue - return Nothing

-- I changed it to return Maybe Winner because if no side is empty there is no winner yet, so I return Nothing.  
-- Can also return NoWinner in this case, and reflect Winner above to be  Winner = Win Player | Tie | NoWinner so 
-- it can return a Winner instead of a Maybe Winner
-- Should only be run on a correct game board, will error out if game board is incorrect
checkWinner :: Game -> Maybe Winner
checkWinner (turn, board@(p1Pits, p1Store@(p1SIndex,p1SMarbles), p2Pits, p2Store@(p2SIndex,p2SMarbles)))
    -- If one side is empty, game ends. Winner is determined by store total counts
    | isSideEmpty (pits P1) || isSideEmpty (pits P2) =
        if p1Total > p2Total then Just (Win P1)
        else if p2Total > p1Total then Just (Win P2)
        else Just Tie
    -- Otherwise no sides are empty - no winner yet
    | otherwise = Nothing
    where
        boardList ::  [(Index, Int)]
        boardList = p1Pits ++ [p1Store] ++ p2Pits ++ [p2Store]

        -- Checks if pits on a side are all empty
        isSideEmpty :: [Index] -> Bool
        isSideEmpty side = null [pieces | (index, pieces) <- boardList, index `elem` side, pieces > 0]

        -- Total of marbles in each players Store 
        p1StoreMarbles = p1SMarbles
        p2StoreMarbles = p2SMarbles
        --p1Store = lookUpUnsafe (store P1) board
        --p2Store = lookUpUnsafe (store P2) board

        -- Total of each players side pits
        --p1SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` (pits P1)]
        --p2SideTotal = sum [ numMarbles | (index,numMarbles) <- board, index `elem` (pits P2)]
        p1SideTotal = sum [ lookUpUnsafe i boardList | i <- pits P1]
        p2SideTotal = sum [ lookUpUnsafe i boardList | i <- pits P2]

        -- If one side is empty, the other player gets all marbles on their side added to their store
        p1Total = if isSideEmpty (pits P2) then p1StoreMarbles + p1SideTotal else p1StoreMarbles
        p2Total = if isSideEmpty (pits P1) then p2StoreMarbles + p2SideTotal else p2StoreMarbles

-----------------------------------------

------------- Story Three ---------------
--This function takes in a game and a move and returns the game board after the fact, without checking if the game is over.

--Passes to completeMoveUnsafe, but will error if the move placed in is valid
completeMove :: Game -> Move -> Maybe Game
completeMove game move = if isValidMove game move then Just (completeMoveUnsafe game move) else Nothing

-- Assumes that the board passed in is in the correct format, as provided by readGame.
-- Completes a move with the following rules: 
    -- You can move marbles from any pit on your side that has marbles in it, but not your store.
    -- Place 1 marble in each pit around the board, placing marbles in your store but not your opponent, until you run out of marbles.
    -- If you land in your store, it's your turn again.
    -- If you land in an empty pit on your side, steal all marbles from that pit and the opponents pit directly across.
completeMoveUnsafe :: Game -> Move -> Game
completeMoveUnsafe game@(turn, board@(p1Pits, p1Store, p2Pits, p2Store)) move 
        | landingIndex == 0  = (P2, distributedBoard) -- Will only ever land on those spaces if it is their turn
        | landingIndex == 7  = (P1, distributedBoard) -- Return the board and make it the player's turn again
        | amountAtIndex == 1 =  case steal distributedBoard of 
            (Just outputBoard) -> (opponent turn, outputBoard) -- Will only ever be returned if it is on the current turn's side
            Nothing            -> (opponent turn, distributedBoard)
        | otherwise = (opponent turn, distributedBoard) -- Otherwise just return the distributed board and change turn 
    where   numMarbles = boardLookUpUnsafe move board -- Figure out how many marbles are in the space
            --Distribute the marbles by calculating how many marbles go in each pit based on updatePit
            distribute@(dP1P, dP1S, dP2P, dP2S) = (map updatePit p1Pits, updatePit p1Store, map updatePit p2Pits, updatePit p2Store)
            --Convert the board of (Bool,Pit) into a traditional board 
            distributedBoard = (map snd dP1P, snd dP1S, map snd dP2P, snd dP2S)
            --Find where it landed, if -1, then it doesn't matter and goes to otherwise
            (landingIndex, amountAtIndex) = findLandingIndexOnBoard distribute 
            
 
            --A function to compute the board given a steal is present at the landingIndex
            steal :: Board -> Maybe Board
            steal (p1Pits, p1Store@(p1StoreIndex, p1StoreMarbles), p2Pits, p2Store@(p2StoreIndex, p2StoreMarbles)) = 
                let opposite = landingIndex + 2*(7-landingIndex) -- Find the opposite index algorithmically
                    (playerSide, opponentSide) = if turn == P1 then (p1Pits,p2Pits) else (p2Pits,p1Pits) -- Figure out whos moving
                    oppositeMarbles = lookUpUnsafe opposite opponentSide -- Figure out how many marbles are in the opposite pit
                    newOpponentSide = changeValue opposite 0 opponentSide -- Clear the opposite pit
                    newPlayerSide   = changeValue landingIndex 0 playerSide -- Clear the landing Pit
                in  if oppositeMarbles == 0 then Nothing else Just (
                    if turn == P1 -- rebuild the board using the changed values, adding the marbles to the correct store
                    then (newPlayerSide, (7,p1StoreMarbles+1+oppositeMarbles), newOpponentSide, p2Store) 
                    else (newOpponentSide, p1Store, newPlayerSide, (0,p2StoreMarbles+1+oppositeMarbles)))

            -- A function to take the output of the updatePit ran on a board and find if the landing index
            -- is important, and then what it is.
            -- Will return (-1,-1) on an unimportant value, will return a value if index is 0 7 or on the side
            findLandingIndexOnBoard :: ([(Bool, Pit)], (Bool, Pit), [(Bool, Pit)], (Bool, Pit)) -> (Index,Int)
            findLandingIndexOnBoard (p1PitTuples, (isLandingP1Store, p1StorePit), p2PitTuples, (isLandingP2Store, p2StorePit)) 
                    | isLandingP1Store = p1StorePit
                    | isLandingP2Store = p2StorePit
                    -- Check the landing index of whoever's turn it is, if it's not present, doesn't matter, return (-1,-1)
                    | otherwise = fromMaybe (-1,-1) (findLandingIndexOnList (if turn == P1 then p1PitTuples else p2PitTuples))
                where 
                    -- Function to find the landing index within a list, and decide if its important
                    findLandingIndexOnList :: [(Bool, Pit)] -> Maybe (Index,Int)
                    findLandingIndexOnList [] = Nothing -- Return Nothing if it isn't found
                    findLandingIndexOnList ((isLandingIndex, landingPit):xs) = 
                        if isLandingIndex 
                        then Just landingPit
                        else findLandingIndexOnList xs

            -- Calculates the distance between two indexes on the board using the described rules
            indexDistance :: Index -> Index -> Int
            indexDistance a b 
                | a <= b     = b - a
                | turn == P1 = 13+(b-a)
                | otherwise  = (14-a)+b-(b `div` 7)

            -- Use the distance to determine how many marbles should be in each space, and return if that's the landing index
            updatePit :: Pit -> (Bool, Pit) 
            updatePit oldPit@(pitIndex, oldMarbles) 
                | move == pitIndex                  = (isLandingPit, (move, numMarbles `div` 13)) 
                | pitIndex == store (opponent turn) = (isLandingPit, oldPit) 
                | otherwise                         = (isLandingPit, newPit)
                where dist = indexDistance move pitIndex
                      distAdd = if dist <= (numMarbles `mod` 13) then 1 else 0
                      newPit =  (pitIndex, oldMarbles + (numMarbles `div` 13) + distAdd)
                      isLandingPit = dist == (numMarbles `mod` 13)

            

-----------------------------------------

------------- Story Four ---------------
--Creates the list of legal moves from a game state
--by checking if the pit is on the current player's side and not empty.
-- Only ever pass in a correct board, will not return intended results if incorrect board.
validMoves :: Game -> [Move]
validMoves (turn, (p1Pits, p1Store, p2Pits, p2Store)) = 
    [index | pit@(index,marbles) <- (if turn == P1 then p1Pits else p2Pits), marbles > 0]

--Extra: Checks if a move is valid given a game (Might be handy for error handling idk)
isValidMove :: Game -> Move -> Bool          
isValidMove game@(turn,board) move 
    | move == 0 = False
    | move == 7 = False
    | move `notElem` (pits turn) = False
    | boardLookUpUnsafe move board == 0 = False
    | otherwise = True

---------------------------------------

------------Story Seventeen------------

rateGame :: Game -> Int
rateGame game@(turn,board@(sideOne,storeOne,sideTwo,storeTwo)) =
    if winner == Nothing
        then sum [turnPoint,storeOnePoints,storeTwoPoints,sideOnePoints,sideTwoPoints]
        else case winner of
             Just (Win P1)  ->  400
             Just (Win P2)  -> -400
             Just Tie ->  0
    where turnPoint      = if turn == P1 then 8 else -8
          storeOnePoints = 8 * (snd storeOne)
          storeTwoPoints = -8 * (snd storeTwo)
          sideOnePoints  = pointsSideTwo sideOne 
          sideTwoPoints  = -1 * pointsSideTwo sideTwo
          winner         = checkWinner game

pointsSideTwo :: [Pit] -> Int
pointsSideTwo [] = 0
pointsSideTwo ((index,marbles):pits) = 
    if marbles==0
        then 8 + (pointsSideTwo pits)
        else weightedMarbles + (pointsSideTwo pits)
    where weightedMarbles = ((((index `mod` 7)-1) `div` 2)+1) * marbles
            -- ^ Yay algebra to get the modifier from the index
{-
-- (Turn,([Pit],Pit,[Pit],Pit))
pointsSide :: Board -> [Index] -> Int
pointsSide board [] = 0
pointsSide board (index:idxs)  
    | index `elem` [1,2,8,9]   = if numMarbles==0 then (8 + pointsSide board idxs) else (3 * numMarbles + pointsSide board idxs)
    | index `elem` [3,4,10,11] = if numMarbles==0 then (8 + pointsSide board idxs) else (2 * numMarbles + pointsSide board idxs)
    | index `elem` [5,6,12,13] = if numMarbles==0 then (8 + pointsSide board idxs) else (1 * numMarbles + pointsSide board idxs)
    | otherwise                = error "pointsSide: The given index is not in a playable side."
    where numMarbles = (lookUpUnsafe index board)
-}

---------------------------------------